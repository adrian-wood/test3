      SUBROUTINE BOGIND(REPORT,VALUES,TYPE,OFT,YMD,TOR,IDENT_PRESSURE)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : BOGIND
!
! PURPOSE       : To BUFR encode, index & store BOGUS data
!
! DESCRIPTION   : Make BUFR message & 23-byte index entry,
!                 call TAFREP to store.
!
! DATA TYPE(S)  : BOGUS
!
! CALLED BY     : BOGUS
!
! CALLS         : ENBUFR, INDLALO, TAFREP                          !1.7
!
! PARAMETERS    : (1) REPORT  - report text                    (i) !1.7
!                 (2) VALUES  - ELEMENTS ARRAY                 (i) !1.7
!                 (3) TYPE    - BOGUS DATATYPE                 (i) !1.7
!                 (4) OFT     - storage FT number     (for TAFREP) !1.7
!                 (5) YMD     - year, month, day               (i) !1.7
!                 (6) TOR     - time of receipt                (i) !1.7
!                 (7) pressure from report (character*4)       (i) !1.7
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bogind.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:02    Sheila Needham  
! $
! Revision 2.1  2002/01/24 10:24:50  usmdb
! Increase size of descriptor array. J.Hodkinson
! Implemented 24/1/2002.
!
! Revision 2.0  2001/05/31  13:27:31  13:27:31  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
!
! Revision 1.7  2000/04/07  09:13:42  09:13:42  usmdb (Generic MetDB account)
! 17 April 2000     C Long
! 1.7  Call INDLALO to put lat/long in index. BOGSTO renamed BOGIND.
!
! Revision 1.6  98/07/23  08:13:15  08:13:15  usmdb (Generic MDB account)
! Make the IDENT unique in each case so that the
! reports aren't chained & have their own index entry
!
! Revision 1.5  98/04/20  06:51:38  06:51:38  usmdb (Generic MDB account)
! Some Upperair BOGUS reports have the same Lat/Long and are rejected
! as duplicates even though the they are at different heights. To
! resolve this the pressure value is placed in the last four bytes
! of the IDENT field in the index entry.                              !D
!
! Revision 1.4  98/01/27  10:09:03  10:09:03  usmdb (Generic MDB account)
! Add a space to the end of the Ident to increase the length to 4
! bytes to match the bitindex for retrieval.                          !C
!
! Revision 1.3  1997/09/25 14:02:50  usjl
! Re-written to improve performance an fix bugs.
!
! Revision 1.2  1997/07/31 09:12:13  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 11:00:13  uspm
! Initial revision
!
! 14/04/97 TO COPE WITH ADDITION OF MINUTES TO THE VALUES ARRAY -
!             INCREASED FROM 12 TO 13                                 !B
!
! 03/03/97 PUT LAT/LONG IN IDENTIFIER FIELD PASSED TO TAFREP TO
!          ENSURE THAT A NEW INDEX ENTRY IS CREATED FOR BOGUS REPORTS
!          THAT HAVE THE SAME ID AND TIME BUT DIFFERENT LOCATIONS.    !A
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!declare character
      CHARACTER REPORT*(*)
      CHARACTER MESSAGE*2000              !Report Text + BUFR message
      CHARACTER ENTRY*23                  !23-byte index entry
      CHARACTER IDENT*9                   !Identifier for index
      CHARACTER ID*4                      !Identifier to be encoded
      CHARACTER TYPE*3                    !Bogus report type
      CHARACTER HEAD*132                  !Version Header
      CHARACTER IDENT_PRESSURE*4          !Pressure for identifier    !D

!declare real
      REAL VALUES(13)                     !Decoded elements           !B
      REAL UNIQUE_ID

!declare integer
      INTEGER TOR(5)
      INTEGER LAT                         !Integer Latitude        !1.7
      INTEGER LONG                        !Integer Longitude       !1.7
      INTEGER BLKSIZ                      !Blksize of dataset
      INTEGER IHOUR                       !Hour of bogus reports
      INTEGER L                           !Length of BUFR message
      INTEGER I                           !Used as loop counter
      INTEGER DATIME(5)                   !Date/Time array for storage
      INTEGER OFT                         !Storage dataset unit number
      INTEGER DESCR(30)                                            !2.1
      INTEGER NDESCR
      INTEGER NVALUES
      INTEGER NOBS
      INTEGER IDES
      INTEGER YMD(3)
      INTEGER IPRESSURE

!save values
      SAVE

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bogind.F,v $
     &'//'$ $Date: 30/01/2006 20:21:02$ $Revision: 1$'

      BLKSIZ=27998

! Character report appears before the BUFR message so insert report.
! Put year, month, day in values array.

      MESSAGE(1:)=REPORT
      VALUES(2)=YMD(1)
      VALUES(3)=YMD(2)
      VALUES(4)=YMD(3)

! Set the local sequence descriptor and other BUFR variables.

      DESCR(1)=IDES(302196)
      NDESCR=1
      NVALUES=13
      NOBS=1
      ID=TYPE

! Call ENBUFR to encode decoded values array into BUFR message

      CALL ENBUFR(DESCR,VALUES,NDESCR,NVALUES,NOBS,ID,
     &            TOR,MESSAGE(81:),.FALSE.,L)

! Don't store report if lat or long is missing.                    !1.7

      IF (VALUES(7).NE.-9999999..AND.VALUES(8).NE.-9999999.) THEN  !1.7
        LAT=VALUES(7)*100      !integer latitude in hundredths     !1.7
        LONG=VALUES(8)*100     !integer longitude in hundredths    !1.7

! Start index entry, leaving times & block/record for TAFREP to fill in.

        IHOUR=VALUES(5)
        ENTRY(3:6)='    '
        ENTRY(7:7)=CHAR(0)           ! Cor number
        ENTRY(8:11)=IDENT_PRESSURE
        ENTRY(12:12)=CHAR(NVALUES)
        CALL INDLALO(ENTRY,VALUES(7),VALUES(8))                    !1.7

! As there are only a few types, and no other suitable identifier,
! add a number depending on the coordinates so that boguses of the
! same type are not treated as obs for the same station and chained.
! (Pressure may be missing, hence the check for zero).

        IDENT(1:5)=TYPE
        READ (IDENT_PRESSURE,'(I4)') IPRESSURE
        IF (IPRESSURE.EQ.0) IPRESSURE=1
        UNIQUE_ID=IPRESSURE*LAT*LONG
        WRITE (IDENT(6:9),'(A4)') UNIQUE_ID

! Put time of data in integer array for TAFREP & store message

        DO I=1,5                                                     !B
          DATIME(I)=VALUES(1+I)     ! INTEGER DATE/TIME              !B
        ENDDO                                                        !B
        CALL TAFREP(DATIME,ENTRY,MESSAGE(:80+L),OFT,BLKSIZ,IDENT)
      ENDIF
      RETURN
      END
