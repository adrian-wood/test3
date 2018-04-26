SUBROUTINE AMDIND(ARRAY,ID,DATIME,ENTRY,TTAAII,CCCC,IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDIND
!
! PURPOSE       : Put time & place elements in index entry
!                 (getting values from an array corresponding to
!                  the BUFR descriptor sequence for encoding, now
!                  311198 - subscripts will have to be changed
!                  if the sequence changes!)
!
! CALLED BY     : AMDAR
!
! CALLS         : INDLALO
!
! ARGUMENTS     : (1) array of decoded values             (input)
!               : (2) aircraft identifier(s)              (input)
!               : (3) date/time of report                 (output)
!               : (4) index entry so far                  (output)
!               : (5) TTAAii from bulletin heading        (input)
!               : (6) collecting centre                   (input)
!               : (7) ident with seconds or flight level  (output)
!                      on end (to go in index entry later)
!
! REVISION INFO :
!
! $Workfile: amdind.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 21/01/2011 14:26:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE indlalo_mod

IMPLICIT NONE

! Subroutine arguments:

REAL,              INTENT(IN)    :: ARRAY(:)   !a1
CHARACTER(LEN=*),  INTENT(IN)    :: ID         !a2
INTEGER,           INTENT(OUT)   :: DATIME(5)  !a3
CHARACTER(LEN=23), INTENT(OUT)   :: ENTRY      !a4
CHARACTER(LEN=6),  INTENT(IN)    :: TTAAii     !a5
CHARACTER(LEN=4),  INTENT(IN)    :: CCCC       !a6
CHARACTER(LEN=9),  INTENT(OUT)   :: IDENT      !a7

! Local declarations:

INTEGER         :: LEVEL
REAL            :: SECOND

!---------------------------------------------------------------------
!save variables
!---------------------------------------------------------------------
SAVE

!
!   The index entry starts as follows:
!
! ____________________________________________________________
! :COR & :hour :minute :ident   :secs :number: lat  : long :
! : F/M  :  6  :   1   :        :or   :of obs:   2  :   2  :
! :flags :bits : byte  :8 bytes :F/L  :1 byte: bytes: bytes:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!           1      2      3-10    11     12   13-14   15-16
!
!
! Set up date/time array for storage program

DATIME(1)=ARRAY(6)    ! YEAR
DATIME(2)=ARRAY(7)    ! MONTH
DATIME(3)=ARRAY(8)    ! DAY
DATIME(4)=ARRAY(9)    ! HOUR
DATIME(5)=ARRAY(10)   ! MINUTE
SECOND=ARRAY(11)

!---------------------------------------------------------------------
!   INDEX ENTRY SECTIONS HANDLED BY THIS ROUTINE ARE:
!
!   MINUTES OF REPORT
!   AIRCRAFT ID (with seconds or flight level on the end)
!   NUMBER OF REPORTS REFERRED TO BY THIS INDEX ENTRY (always 1)
!   LATITUDE AND LONGITUDE
!
!   INDEX ENTRY SECTIONS TO BE DONE BY NEXT ROUTINE ARE:
!
!   COR FLAG
!   FINE MESH FLAG MAY NOT BE SET ANYWHERE
!   TYPE FLAGS - MAY BE SET IN FUTURE
!   REPORT FLAGS - MAY BE SET IN FUTURE
!   TIME OF RECEIPT
!   REC NUM
!   BLOCK NUMBER
!---------------------------------------------------------------------

ENTRY(2:2)=CHAR(DATIME(5)) !   LOAD IN MINUTES OF REPORT

ENTRY(3:11)=TTAAii(1:4)//CHAR(0)//CCCC

ENTRY(12:12)=CHAR(1)   ! NUMBER OF REPORTS IN SEQUENCE

!---------------------------------------------------------------------
! For BUFR reports set the last byte of the identifier to seconds.
! For character data (seconds not reported) set the last byte to
! the flight level (unless it's too big for 8 bits).
! The flight level should distinguish between reports during
! ascent & descent when several obs can have the same lat, long &
! minute but different flight levels.  In level flight (FL>300)
! there should be no need for this extra coordinate in the index.
! (0.005 is a rounding factor to get exactly the reported FL,
! converted from hundreds of feet to metres by *30.48)
! For negative flight levels (ARRAY(15) <= 0), set LEVEL=0
!---------------------------------------------------------------------

IF (ID(9:9) >= 'A'.AND.ID(9:9) <= 'Z') THEN
  IDENT(1:8)=ID(9:16)
ELSE
  IDENT(1:8)=ID(1:8)
END IF

IF (SECOND >= 0) THEN          ! seconds reported: BUFR data
  IDENT(9:9)=CHAR(INT(SECOND))
ELSE                           ! seconds missing: characters
  IF (ARRAY(15) > 0.0) THEN
    LEVEL=(ARRAY(15)+0.005)/30.48
    IF (LEVEL > 255) LEVEL=255
  ELSE
    LEVEL=0
  END IF
  IDENT(9:9)=CHAR(LEVEL)
END IF

!---------------------------------------------------------------------
! Lat & long put in index entry as 2-byte integers (hundredths)
!---------------------------------------------------------------------

CALL INDLALO(ENTRY,ARRAY(13),ARRAY(14))

RETURN
END SUBROUTINE AMDIND
