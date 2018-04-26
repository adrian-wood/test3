      SUBROUTINE TRKSTO(REPORT,CCCC,SingleReportLength,
     &                  ICCCC,CORNUM,TTAAII,TOR,NUM_PART,
     &                  ELEMS_ARRAY,CALL_SIGN,IFT)                  !2.0

!-----------------------------------------------------------------------
!
! ROUTINE       : TRKSTO
!
! PURPOSE       : To BUFR encode and store a TRACKOB observation.
!
! DESCRIPTION   : Calls ENBUFR to BUFR encode a TRACKOB observation.
!               : The report is then stored in the MetDB by TAFREP.
!
! DATA TYPE(S)  : TRACKOB (FM-62-VIII)
!
! CALLED BY     : TRKEXC
!
! CALLS         : ENBUFR, TAFREP, INDLALO                           !1.4
!
! ARGUMENTS     : (1)  Report from TRKBUL
!               : (2)  Character collecting centre
!               : (3)  Length of report
!               : (4)  Integer collecting centre
!               : (5)  Correction flag
!               : (6)  TTAAII
!               : (7)  Time Of Receipt
!               : (8)  Part identifier
!               : (9)  Array of values to BUFR encode
!               : (10) Ship call sign
!               : (11) output unit number
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:30$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trksto.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:30    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:28  usmdb
! Removed unused dummy arguments PTR and ISTAT. Removed unused local
! variables, added copyright and modified header - S.Cox
!
! Revision 1.4  2000/04/07  09:26:20  09:26:20  usmdb (Generic MetDB account)
! 17 April 2000      C Long
! Call INDLALO to put lat/long in index.
! 
! Revision 1.3  2000/03/10  09:56:30  09:56:30  usmdb (Generic MDB account)
! 20 March 2000     C Long
! Don't lose last letter of CCCC in trailer
!
! Revision 1.2  99/01/14  13:51:46  13:51:46  usmdb (Generic MDB account)
! 18-01-99 S.Cox
! Don't pass variable CALL_SIGN to ENBUFR as ENBUFR converts it to ASCII
! and CALL_SIGN needs to be reused.
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!----------------------------------------------------------------------
! declare character
!----------------------------------------------------------------------

      CHARACTER REPORT*(*)          !- Single Trackob report
      CHARACTER CCCC*(*)            !- Collecting Centre code
      CHARACTER CALL_SIGN*(*)       !- Ship Callsign of report
      CHARACTER MESAGE*2000
      CHARACTER ENTRY*23            !- Index entry
      CHARACTER IDENT*10            !- Report ID
      CHARACTER NAMES*9             !- Characters to BUFR encode    !1.2
      CHARACTER TTAAII*(*)
      CHARACTER CORNUM*(*)
      CHARACTER HEAD*132                                            !1.2

!----------------------------------------------------------------------
! declare integer
!----------------------------------------------------------------------

      INTEGER IHOUR                 !- Report hour for TAFREP
      INTEGER BLKSIZ
      INTEGER I                     !- Used in datime/TOR loop
      INTEGER IFT                   !- Storage dataset FT no
      INTEGER TOR(5)                !- Time of receipt array
      INTEGER NDESCR
      INTEGER NOBS
      INTEGER DESCR(500)
      INTEGER START
      INTEGER LENG
      INTEGER ICCCC
      INTEGER DATIME(5)
      INTEGER L                     !- Length of BUFR message
      INTEGER NUM_PART              !- No. of groups decoded
      INTEGER IDES
      INTEGER SingleReportLength

!----------------------------------------------------------------------
! declare real
!----------------------------------------------------------------------

      REAL ELEMS_ARRAY(17)          !- Decoded elements array

!----------------------------------------------------------------------
! Save variables
!----------------------------------------------------------------------

      SAVE

!----------------------------------------------------------------------
! initialize variables
!----------------------------------------------------------------------

      DO I=1,500
        DESCR(I)=-9999999
      ENDDO
      START=0
      DESCR(1)=IDES(303192)
      BLKSIZ=27998
      MESAGE(:)=' '
      IDENT(:)=' '
      IDENT(1:9)=CALL_SIGN
      NAMES(1:9)=CALL_SIGN                                          !1.2

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/trksto.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:25:30$'

!----------------------------------------------------------------------
! Put report in characters at start of string to be stored
!----------------------------------------------------------------------

      LENG=SingleReportLength
      MESAGE(1:LENG)=REPORT(:SingleReportLength)

!----------------------------------------------------------------------
! Now call ENBUFR to encode the elements into BUFR
!----------------------------------------------------------------------

      NDESCR=1
      NOBS=1
      CALL ENBUFR(DESCR,ELEMS_ARRAY,NDESCR,17,NOBS,NAMES,           !1.2
     &            TOR,MESAGE(LENG+1:),.FALSE.,L)

!----------------------------------------------------------------------
! put cccc in section 1 of the bufr message, setting data type too
! (displacement as for bufr version 1; change if total length at start)
!----------------------------------------------------------------------

      IF (CCCC.NE.' ') THEN
        MESAGE(LENG+9:LENG+9)=CHAR(ICCCC/256)
        MESAGE(LENG+10:LENG+10)=CHAR(MOD(ICCCC,256))
      ENDIF

      MESAGE(LENG+13:LENG+13)=CHAR(3)

      IHOUR=ELEMS_ARRAY(5)      !hour of data
      IF (ELEMS_ARRAY(7).GT.-9999999. .AND.                        !1.4
     &    ELEMS_ARRAY(8).GT.-9999999.) THEN                        !1.4

!----------------------------------------------------------------------
! start index entry, leaving tor & block/record for tafrep to fill in
! n.b. the hour here will be changed to one relative to the index hour
! and the bulletin details are for the trailer, they will be replaced
! by the identifier in the index entry itself.
!----------------------------------------------------------------------

        ENTRY(1:2)=CHAR(IHOUR)//CHAR(0) !TAFREP SETS RELATIVE HOUR
        ENTRY(3:11)=TTAAII(1:4)//CORNUM(2:2)//CCCC                 !1.3
        ENTRY(12:12)=CHAR(NUM_PART)
        CALL INDLALO(ENTRY,ELEMS_ARRAY(7),ELEMS_ARRAY(8))          !1.4

!----------------------------------------------------------------------
! put time of data in integer array for tafrep & store report+message
!----------------------------------------------------------------------

        DO I=1,5
          DATIME(I)=ELEMS_ARRAY(I+1)
        ENDDO

        IF (DATIME(5).EQ.-9999999) DATIME(5)=0   !MINS MISSING? MINS=0

!----------------------------------------------------------------------
! do not pass report to TAFREP if there is any doubt about the time as
! TAFREP expects the date/time to be correct
!----------------------------------------------------------------------

        IF ((DATIME(1).NE.-9999999).AND.(DATIME(2).GE.1) .AND.
     &     (DATIME(2) .LE. 12) .AND. (DATIME(3) .GE. 1) .AND.
     &     (DATIME(3) .LE. 31) .AND. (DATIME(4) .GE. 0) .AND.
     &     (DATIME(4) .LE. 23)) THEN

            CALL TAFREP(DATIME,ENTRY,MESAGE(:LENG+L),IFT,BLKSIZ,
     &                IDENT)
        ENDIF                                                      !1.4
      ENDIF

      RETURN                          !return to synopt for next
      END                             !bulletin
