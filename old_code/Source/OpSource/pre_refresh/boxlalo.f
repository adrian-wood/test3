      SUBROUTINE BOXLALO (BOX, CBOX)

!-----------------------------------------------------------------------
!
! PROGRAM       : BOXLALO
!
! PURPOSE       : To convert the minimum and maximum latitudes and
!                 longitudes for a group of observations into a 4-byte
!                 string of area boundary values as required for index
!                 entries.
!
! DESCRIPTION   : BOXLALO does for a group of observations what INDLALO
!                 does for a single observation. It takes as input a
!                 4-element array "BOX" containing the minimum and
!                 maximum latitudes and longitudes (as output by
!                 subroutine LATBOX) for a group of obs and converts
!                 the values into a 4-byte string "CBOX" giving the
!                 boundaries of an area containing all the obs. The
!                 output is in the format used in index entries.
!
!                 The format of 12-byte and 23-byte index entries is
!                 documented in MetDB Technical Note 2 located at
!                 http://www01/metdb/documentation/technotes/dmtn2.html
!                 (or via links from the MetDB Home Page). See the
!                 tables in sections 2.2 and 2.3 of that document. The
!                 4-byte string goes in bytes 5-8 of 12-byte index
!                 entries (section 2.2) and bytes 13-16 of 23-byte
!                 index entries (section 2.3).
!
!                 Elements of BOX and bytes in CBOX are in the order
!                 'minimum latitude', 'maximum latitude', 'minimum
!                 longitude' and 'maximum longitude'. In CBOX, latitude
!                 boundaries are in units of degrees north of 90S;
!                 longitudes are in 2-degree units measured east from
!                 180W. Thus all bytes have values in the range 0-180.
!
!                 It is important to ensure that the values in BOX are
!                 rounded 'outwards' so that rounding to integers does
!                 not exclude observations close to the boundary. This
!                 routine was written because several storage routines
!                 did not do this rounding correctly.
!
! PARAMETERS    : 1. BOX   (Input) Real array of dimension 4 with area
!                            limits for a group of observations.
!                 2. CBOX  (Output) CHARACTER*4 string with area
!                            boundaries for insertion in index entry.
!
! CALLED BY     : ERSIND, SFERICS, SFLOC, STBIND.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/boxlalo.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:03    Sheila Needham  
! $
! Revision 1.1  2005/03/17 11:42:20  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER     J        ! Loop counter
      INTEGER     IBOX(4)  ! Rounded box boundaries for area limits
      REAL        BOX(4)   ! Area lat/long limits (as output by LATBOX)
      CHARACTER*4 CBOX     ! String for insertion in index entry
      CHARACTER*132 HEAD   ! String to hold revision information
      LOGICAL     FIRST    ! Flag for first call to this routine

      DATA FIRST /.TRUE./
!                                                 Revision information
      IF (FIRST) THEN
        HEAD = '$RCSfile: boxlalo.f,v $ ' //
     &         '$Revision: 1$  %Date: $'
        FIRST = .FALSE.
      END IF

!-----------------------------------------------------------------------
!   The maximum and minimum latitudes and longitudes in BOX are here
! converted into area boundary numbers ensuring that the rounding is
! outwards, i.e. minimum values are rounded down and maximum values
! rounded up.
!   Minimum values are converted into area boundary values in the range
! 0-180 and and the INT function is applied to round downwards.
!   Maximum values are also converted into area boundary values (n) but
! the INT function is applied to (180-n) and the result subtracted from
! 180 to round upwards.
!   It is not necessary to do anything different for areas which
! straddle the 180 degree meridian.
!-----------------------------------------------------------------------
!                                                     Minimum latitude
      IBOX(1) = INT(BOX(1)+90.0)
!                                                     Maximum latitude
      IBOX(2) = 180 - INT(90.0-BOX(2))
!                                                     Minimum longitude
      IBOX(3) = INT(0.5*BOX(3)+90.0)
!                                                     Maximum longitude
      IBOX(4) = 180 - INT(90.0-0.5*BOX(4))

!-----------------------------------------------------------------------
!   Bad data (box numbers outside the range 0-180) are replaced with
! extreme values (0 or 180 as appropriate) to ensure that any good data
! in the message being indexed can be retrieved. If an alternative
! action is required (e.g. rejection of messages with bad lat/longs)
! this will need to be arranged by the programmer outside this routine.
!-----------------------------------------------------------------------
!                                                    Check for bad data
      DO J=1,4
        IF (ABS(IBOX(J)).LT.0.0) IBOX(J) = 0
        IF (ABS(IBOX(J)).GT.180.0) IBOX(J) = 180
      END DO ! J

!-----------------------------------------------------------------------
!   Make 4-byte string for index entry and return.
!-----------------------------------------------------------------------

      CBOX = CHAR(IBOX(1)) // CHAR(IBOX(2)) //
     &       CHAR(IBOX(3)) // CHAR(IBOX(4))
      RETURN
      END
