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
! $Revision: 1$
! $Date: 30/01/2006 20:22:31$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getare.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:31    Sheila Needham  
! $
! Revision 2.1  2001/02/06 12:16:45  usmdb
! Removed range checking for RPOLE. It is valid to have
! a RPOLE outside a normal lat/lon chart area - S.Cox
!
! Revision 2.0  2001/01/08  11:58:39  11:58:39  usmdb (Generic MDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/08/04  13:09:54  13:09:54  uspm (Pat McCormack)
! First revisioned version for  1  - with Y2K change
!
! Revision 1.2  1997/02/12 12:30:04  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/11 14:35:26  uspm
! Initial revision
!
! 28-07-97  !A  : Addition of code to deal with a rotated lat/lon area
!               : and a rotated pole. Integer variable IAREA has been
!               : changed to real variable AREA - J.Lewthwaite
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

        IMPLICIT NONE                                                 !A

!-----------------------------------------------------------------------
! Declare integer variables (in aplhabetical order)
!-----------------------------------------------------------------------

      INTEGER  GRPLEN     !- Length of group being decoded
      INTEGER  I
      INTEGER  IFAIL      !- Indicates failure in decode
      INTEGER  ILEN
      INTEGER  IPOS       !- Pointer within users request strng

!-----------------------------------------------------------------------
! Declare real variables (in aplhabetical order)
!-----------------------------------------------------------------------

      REAL     AREA(5)    !- Users Lat/Long area                      !A
      REAL     RPOLE(2)   !- Lat/Long of rotated pole

!-----------------------------------------------------------------------
! Declare character variables (in aplhabetical order)
!-----------------------------------------------------------------------

      CHARACTER*(*)  CERR     !- Error messages
      CHARACTER*(*)  REQ      !- Users request
      CHARACTER*132 HEAD

!-----------------------------------------------------------------------
! SAVE to ensure variables still set on re-entry
!-----------------------------------------------------------------------

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/getare.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:22:31$ '

!-----------------------------------------------------------------------
! Start decode of users AREA
!-----------------------------------------------------------------------

      IF (AREA(1).EQ.-9.0) THEN                 !- Normal Lat/Long grid
        IF(REQ(IPOS:IPOS+7).EQ.'REGIONAL')THEN
          AREA(1)=-1.0                          !- Indicate Regional
        ELSEIF(REQ(IPOS:IPOS+5).EQ.'GLOBAL')THEN
          AREA(1)=-2.0                          !- Indicate GLOBAL
        ELSE
          AREA(1)=0.0

          I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
          IF(I.EQ.0)THEN                                              !A
            CERR=' NO LATITUDE'                                       !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Top left hand latitude
!-----------------------------------------------------------------------

          IF (GRPLEN .LT. 2) THEN                                     !A
            CERR=' '                                                  !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(2)

!-----------------------------------------------------------------------
! Check sign of Latitude
!-----------------------------------------------------------------------

          IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1).EQ.'S') THEN            !A
            AREA(2)=AREA(2)*(-1.0)                                    !A
          ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE.'N') THEN      !A
            CERR=' INVALID SIGN '                                     !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF

!-----------------------------------------------------------------------
! Increment IPOS past first Lat group
!-----------------------------------------------------------------------

          IPOS=IPOS+GRPLEN+1                                          !A
          I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
          IF(I.EQ.0)THEN                                              !A
            CERR=' NO LATITUDE'                                       !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Bottom right hand Latitude
!-----------------------------------------------------------------------

          IF (GRPLEN .LT. 2) THEN                                     !A
            CERR=' '                                                  !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(4)                      !A

!-----------------------------------------------------------------------
! Check sign of Latitude
!-----------------------------------------------------------------------

          IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1).EQ.'S') THEN            !A
            AREA(4)=AREA(4)*(-1.0)                                    !A
          ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE.'N') THEN      !A
            CERR=' INVALID SIGN '                                     !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF

!-----------------------------------------------------------------------
! Increment IPOS past second Lat group
!-----------------------------------------------------------------------

          IPOS=IPOS+GRPLEN+1                                          !A
          I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
          IF(I.EQ.0)THEN                                              !A
            CERR=' NO LONGITUDE'                                      !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Top left hand Longitude
!-----------------------------------------------------------------------

          IF (GRPLEN .LT. 2) THEN                                     !A
            CERR=' '                                                  !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(3)

!-----------------------------------------------------------------------
! Check sign of Longitude
!-----------------------------------------------------------------------

          IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1).EQ.'W') THEN            !A
            AREA(3)=AREA(3)*(-1.0)                                    !A
          ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE.'E') THEN      !A
            CERR=' INVALID SIGN '                                     !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A

!-----------------------------------------------------------------------
! Increment IPOS past second Lat group
!-----------------------------------------------------------------------

          IPOS=IPOS+GRPLEN+1                                          !A
          I=INDEX(REQ(IPOS:ILEN),' ')                                 !A
          IF(I.EQ.0)THEN                                              !A
            CERR=' NO LONGITUDE'                                      !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          GRPLEN=I-1                                                  !A

!-----------------------------------------------------------------------
! Bottom right hand Longitude
!-----------------------------------------------------------------------

          IF (GRPLEN .LT. 2) THEN                                     !A
            CERR=' '                                                  !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A
          READ(REQ(IPOS:IPOS+GRPLEN-2),*)AREA(5)                      !A

!-----------------------------------------------------------------------
! Check sign of Longitude
!-----------------------------------------------------------------------

          IF(REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1).EQ.'W') THEN            !A
            AREA(5)=AREA(5)*(-1.0)                                    !A
          ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE.'E') THEN      !A
            CERR=' INVALID SIGN '                                     !A
            IFAIL=8                                                   !A
            GOTO 999                                                  !A
          ENDIF                                                       !A

!-----------------------------------------------------------------------
! Increment IPOS past second Longitude group or set it to ILEN
!-----------------------------------------------------------------------

          IPOS=IPOS+GRPLEN+1                                          !A
          IF(IPOS.GT.ILEN) THEN                                       !A
            IPOS=ILEN                                                 !A
          ENDIF                                                       !A

        ENDIF

!-----------------------------------------------------------------------
! Rotated Lat Long pole
!-----------------------------------------------------------------------

      ELSEIF (AREA(1).EQ.-4.0) THEN             !- Rotated Pole       !A
        AREA(1)=1.0                                                   !A
        I=INDEX(REQ(IPOS:ILEN),' ')                                   !A
        IF(I.EQ.0)THEN                                                !A
          CERR=' NO LATITUDE'                                         !A
          IFAIL=8                                                     !A
          GOTO 999                                                    !A
        ENDIF                                                         !A
        GRPLEN=I-1                                                    !A

!-----------------------------------------------------------------------
! Read in Latitude of pole
!-----------------------------------------------------------------------
                                                                      !A
        READ(REQ(IPOS:IPOS+GRPLEN-2),*)RPOLE(1)                       !A

!-----------------------------------------------------------------------
! Check sign of LATITUDE
!-----------------------------------------------------------------------

        IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .EQ. 'S') THEN           !A
          RPOLE(1)=-RPOLE(1)                                          !A
        ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE. 'N') THEN       !A
          IFAIL=8                                                     !A
          CERR='SIGN OF RPOLE LATITUDE INVALID'                       !A
          GOTO 999                                                    !A
        ENDIF                                                         !A

!-----------------------------------------------------------------------
! Increment pointer onto Longitude group
!-----------------------------------------------------------------------

        IPOS=IPOS+GRPLEN+1                                            !A
        I=INDEX(REQ(IPOS:ILEN),' ')                                   !A
        IF(I.EQ.0)THEN                                                !A
          CERR=' NO LATITUDE'                                         !A
          IFAIL=8                                                     !A
          GOTO 999                                                    !A
        ENDIF                                                         !A
        GRPLEN=I-1                                                    !A

!-----------------------------------------------------------------------
! Read in Longitude of Pole
!-----------------------------------------------------------------------

        READ(REQ(IPOS:IPOS+GRPLEN-2),*)RPOLE(2)                       !A

!-----------------------------------------------------------------------
! Check sign of LONGITUDE
!-----------------------------------------------------------------------

        IF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .EQ. 'W') THEN           !A
          RPOLE(2)=-RPOLE(2)                                          !A
        ELSEIF (REQ(IPOS+GRPLEN-1:IPOS+GRPLEN-1) .NE. 'E') THEN       !A
          IFAIL=8                                                     !A
          CERR='SIGN OF RPOLE LONGITUDE INVALID'                      !A
          GOTO 999                                                    !A
        ENDIF

!-----------------------------------------------------------------------
! End of Rotated Lat Long Pole section
!
! Increment onto next group or set IPOS to ILEN
!-----------------------------------------------------------------------

        IPOS=IPOS+GRPLEN+1
        IF(IPOS.GT.ILEN) THEN
          IPOS=ILEN
        ENDIF
      ENDIF

 999  CONTINUE

      RETURN
      END
