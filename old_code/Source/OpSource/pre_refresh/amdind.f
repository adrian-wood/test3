      SUBROUTINE AMDIND(ARRAY,ID,DATIME,ENTRY,TTAAII,CCCC,IDENT)    !2.0

      IMPLICIT NONE                                                   !B

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDIND
!
! PURPOSE       : Put time & place elements in index entry          !1.3
!                 (getting values from an array corresponding to    !1.3
!                  the BUFR descriptor sequence for encoding, now   !1.3
!                  311198 - subscripts will have to be changed      !1.3
!                  if the sequence changes!)                        !1.3
!
! CALLED BY     : AMDAR
!
! CALLS         : INDLALO                                           !1.4
!
! PARAMETERS    : (1) array of decoded values             (input)   !1.3
!               : (2) aircraft identifier(s)              (input)   !1.5
!               : (3) date/time of report                 (output)  !1.3
!               : (4) index entry so far                  (output)  !1.3
!               : (5) TTAAii from bulletin heading        (input)  !1.4a
!               : (6) collecting centre                   (input)   !1.3
!               : (7) ident with seconds or flight level  (output)  !1.3
!                      on end (to go in index entry later)          !1.3
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/amdind.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:54    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:30  usmdb
! Removed unused dummy argument CORF. Removed unused local variable
! I. Added copyright and modified header - S.Cox
!
! Revision 1.5  2000/05/05  10:35:36  10:35:36  usmdb (Generic MetDB account)
! 15 May 2000    C Long
! Index under aircraft number if aircraft & flight number both given 
! 
! Revision 1.4  2000/04/07  09:28:21  09:28:21  usmdb (Generic MDB account)
! 15 May 2000      C Long
! Call INDLALO to put lat/long in index entry. Put TTAA from bulletin
! heading in trailer.
!
! Revision 1.3  99/09/16  15:07:13  15:07:13  usmdb (Generic MDB account)
! 20 Sept 99    C Long
! Put flight level rather than seconds at end of identifier in index
! entry for character AMDARs. Change subscripts to fit sequence with
! seconds included. Remove useless code & update comments.
!
! Revision 1.2  97/07/31  09:10:11  09:10:11  uspm (Pat McCormack)
! First revision for  1
!
! 07/07/97  ADD SECOND TO ARGUMENT LIST. SECOND FROM BUFR MESSAGE
!           USED IN INDEX ENTRY                                       !E
!
! Revision 1.1  1997/07/04 10:42:22  uspm
! Initial revision
!
! 30/06/97  Set seconds in index entry for chaining purposes.
!           change to method to get hold of Aircraft ID. Remove
!           code that set-up Lat/Long box as only single report
!           now stored.                                               !D
!
! 22/01/97  Correct last change: lat/long are 12-13, not 11-12!       !c
!
! 28/11/96  INCLUSION OF IMPLICIT NONE AND ALL VARIABLES DECLARED     !B
!           CHANGE LAT LONG CALCULATIONS TO PRODUCE CORRECT LAT
!           LONG BOX WITH A CALL TO LATBOX.                           !A
!
! 13/06/94  CHECK FOR MISSING AIRCRAFT IDENTIFIER
!
! 14/08/92  INCREASE ARRAY SIZES TO COPE WITH LARGER BULLETINS,
!           AND SO MORE REPORTS.
!
! 24/03/92  CHANGE TO THE CONVERSION DATASET REQUIRED ARRAY
!           POSITIONS FOR DATE/TIMES AND LATS/LONGS TO BE UPDATED.
!
! 10/03/92  IF ONLY 1 REPORT IN MESSAGE, CHANGE LAT/LONG BOX
!           TO SINGLE POINT LAT/LONG, IN CORRECT UNITS, ALLOWING
!           UNIFIED BOXED/NONBOXED RETRIEVAL.
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

!--------------------------------------------------------------------
!Declare integer
!--------------------------------------------------------------------

      INTEGER DATIME(5)
      INTEGER LEVEL                                                !1.3

!---------------------------------------------------------------------
!declare real
!---------------------------------------------------------------------

      REAL ARRAY(*)
      REAL SECOND                                                   !E

!---------------------------------------------------------------------
!declare character
!---------------------------------------------------------------------

      CHARACTER ENTRY*23
      CHARACTER ID*(*)
      CHARACTER CCCC*4
      CHARACTER TTAAii*6                                           !1.4a
      CHARACTER IDENT*9
      CHARACTER HEAD*132

!---------------------------------------------------------------------
!save variables
!---------------------------------------------------------------------
      SAVE

!---------------------------------------------------------------------
!initialize variables
!---------------------------------------------------------------------
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/amdind.F,v $
     &'//'$ $Date: 30/01/2006 20:20:54$ $Revision: 1$'

!
!   The index entry starts as follows:                           !1.3
!
! ____________________________________________________________   !1.3
! :COR & :hour :minute :ident   :secs :number: lat  : long :     !1.3
! : F/M  :  6  :   1   :        :or   :of obs:   2  :   2  :     !1.3
! :flags :bits : byte  :8 bytes :F/L  :1 byte: bytes: bytes:     !1.3
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   !1.3
!           1      2      3-10    11     12   13-14   15-16      !1.3
!
!
! Set up date/time array for storage program                      !1.3

      DATIME(1)=ARRAY(6)    ! YEAR
      DATIME(2)=ARRAY(7)    ! MONTH
      DATIME(3)=ARRAY(8)    ! DAY
      DATIME(4)=ARRAY(9)    ! HOUR
      DATIME(5)=ARRAY(10)   ! MINUTE
      SECOND=ARRAY(11)                                            !1.3

!---------------------------------------------------------------------
!   INDEX ENTRY SECTIONS HANDLED BY THIS ROUTINE ARE:
!
!   MINUTES OF REPORT                                             !1.3
!   AIRCRAFT ID (with seconds or flight level on the end)         !1.3
!   NUMBER OF REPORTS REFERRED TO BY THIS INDEX ENTRY (always 1)  !1.3
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

      ENTRY(3:11)=TTAAii(1:4)//CHAR(0)//CCCC                       !1.4a

      ENTRY(12:12)=CHAR(1)   ! NUMBER OF REPORTS IN SEQUENCE

!---------------------------------------------------------------------
! For BUFR reports set the last byte of the identifier to seconds. !1.3
! For character data (seconds not reported) set the last byte to   !1.3
! the flight level (unless it's too big for 8 bits).               !1.3
! The flight level should distinguish between reports during       !1.3
! ascent & descent when several obs can have the same lat, long &  !1.3
! minute but different flight levels.  In level flight (FL>300)    !1.3
! there should be no need for this extra coordinate in the index.  !1.3
! (0.005 is a rounding factor to get exactly the reported FL,      !1.3
! converted from hundreds of feet to metres by *30.48)             !1.3
! For negative flight levels (ARRAY(15).LE.0), set LEVEL=0         !1.3
!---------------------------------------------------------------------

      IF (ID(9:9).GE.'A'.AND.ID(9:9).LE.'Z') THEN                  !1.5
        IDENT(1:8)=ID(9:16)                                        !1.5
      ELSE                                                         !1.5
        IDENT(1:8)=ID(1:8)                                         !1.5
      ENDIF                                                        !1.5

      IF (SECOND.GE.0) THEN          ! seconds reported: BUFR data !1.3
        IDENT(9:9)=CHAR(INT(SECOND))                               !1.5
      ELSE                           ! seconds missing: characters !1.3
        IF (ARRAY(15).GT.0.0) THEN                                 !1.3
          LEVEL=(ARRAY(15)+0.005)/30.48                            !1.3
          IF (LEVEL.GT.255) LEVEL=255                              !1.3
        ELSE                                                       !1.3
          LEVEL=0                                                  !1.3
        ENDIF                                                      !1.3
        IDENT(9:9)=CHAR(LEVEL)                                     !1.5
      ENDIF                                                        !1.3

!---------------------------------------------------------------------
! Lat & long put in index entry as 2-byte integers (hundredths)    !1.3
!---------------------------------------------------------------------

      CALL INDLALO(ENTRY,ARRAY(13),ARRAY(14))                       !1.4

      RETURN
      END
