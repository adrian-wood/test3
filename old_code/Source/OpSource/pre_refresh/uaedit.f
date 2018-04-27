      SUBROUTINE UAEDIT (REPORT,TTAAII,CCCC,ICCCC,YYGGGG,CORN,IFT)    !G

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : UAEDIT                                              
!                                                                     
! PURPOSE       : MAIN PROGRAM FOR UPPER AIR STORAGE: CALLS           
!                 EXPANSION, BUFR ENCODE & MDB STORAGE ROUTINES       
!                                                                     
! DESCRIPTION   : EXPANDS, ENCODES & STORES A REPORT                  
!                                                                     
! DATA TYPE(S)  : UPPER AIR (TEMPS, PILOTS & DROPSONDES)              
!                                                                     
! CALLED BY     : UABUL (WHICH READS BULLETINS & PASSES ONE REPORT    
!                                                                     
! CALLS         : UAXPAND, ENBUFR, TAFREP (FOR EACH OB), UASORT,   !1.8
!                 INDLALO                                          !1.8
!                                                                     
! PARAMETERS    : (1) REPORT (with length set)                        
!                 (2) TTAAII                                          
!                 (3) COLLECTING CENTRE                               
!                 (4) COLLECTING CENTRE NUMBER                        
!                 (5) DATIME GROUP                                    
!                 (6) CORRECTION NUMBER                               
!                 (7) FT NUMBER OF DATASET                            
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uaedit.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:37    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:32  usmdb
! Removed unused variables. Changed lines where subscript 1 of array
! A is not of type INTEGER by using the NINT function. Added
! copyright and modified header - S.Cox
!
! Revision 1.9  2000/08/09  14:56:04  14:56:04  usmdb (Generic MetDB account)
! 21 Aug 2000   C Long
! If minute not reported, set it to missing, not zero.
! 
! Revision 1.8  2000/04/07  09:26:02  09:26:02  usmdb (Generic MDB account)
! 17 April 2000      C Long
! Call INDLALO to put lat/long in index.
!
! Revision 1.7  99/06/10  15:21:03  15:21:03  usmdb (Generic MDB account)
! Put REJ on end of identifier Chris Long 21 June 1999
!
! Revision 1.6  99/02/11  11:49:56  11:49:56  usmdb (Generic MDB account)
! 15-02-1999. Correct the number of levels information being passed to
! enbufr. Jon Lewthwaite
! 1.6a Store obs even if no lat/long   C Long
!
! Revision 1.5  98/02/19  11:48:11  11:48:11  usmdb (Generic MDB account)
! Remove special code for dropsond reports as these are now
! handled like TEMPS/PILOTS                                           !H
!
! Revision 1.4  1998/01/27 14:05:07  usmdb
! Intermediate change to correctly calculate length of report.
! Change made for IBM release before software was made generally
! available
!
! 02/01/98 - Change argument list to pass length of report            !G
!            Avoid resetting length from = which may not be there!    !G
!
! Revision 1.3  1997/09/22 14:05:38  uspm
! Ensure all labelled statements are CONTINUE statements
!
! Revision 1.2  1997/07/31 11:44:56  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 14:37:40  uspm
! Initial revision
!
! 17/03/97 Add TB17BIT5 to indicate whether a PILOT report had a
!          pressure sensor or not. This is then set in the trailer
!          (byte17, bit5) for retrieval                               !F
!
! 20/01/97 CHANGE INDEX ENTRY AND TRAILER TO INDICATE THAT
!          TEMPS/PILOTS IN SAME CHAIN                                 !A
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

!declare characters
      CHARACTER   REPORT*(*)     !raw bulletin from synopt
      CHARACTER   TTAAII*6       !report header identifier
      CHARACTER   YYGGGG*6       !date/time in bull header
      CHARACTER   CORN*2         !cor number
      CHARACTER   TT*2
      CHARACTER   IDENT*10       !ident(10:10)contains btye17 index
      CHARACTER   ID*9           !report id
      CHARACTER   MESAGE*5000    !raw report
      CHARACTER   ENTRY*23       !23 byte index entry
      CHARACTER   B17BIT*1       !index entry byte 17 1bit values
      CHARACTER   CCCC*4
      CHARACTER   TYPE*2
      CHARACTER   HEAD*132       !revision information

!declare integers
      INTEGER     PART_TYPE     !indicates partA,B,C or D
      INTEGER     DESCR(2000)   !array of descriptors used in enbufr
      INTEGER     B17_BIT8      !byte17 bit8 from index for checking
      INTEGER     PTR           !pointer within message
      INTEGER     CORNUM        !integer cor number
      INTEGER     TOR(5)        !time of reciept array
      INTEGER     NOW(8)        !day/time array from system
      INTEGER     DATIME(5)     !day/time array from decode
      INTEGER     NDESCR        !number of bufr descriptors
      INTEGER     ICCCC         !collecting centre BUFR code table no.
      INTEGER     I,J           !used for loop counting
      INTEGER     K             !used in initialization loops
      INTEGER     LENG          !length of message
      INTEGER     NOBS          !number of observations passed to enbufr
      INTEGER     L             !total length of BUFR message
      INTEGER     IFT           !dataset ft number
      INTEGER     BLKSIZ        !blksize of storage dataset
      INTEGER     IHOUR
      INTEGER     NL
      INTEGER     IVALUE
      INTEGER     START
      INTEGER     NUM_LEVS      !count of good levels decoded
      INTEGER     MIN
      INTEGER     TB17BIT5      !Trailer Byte17 Bit5                !F
      INTEGER     IDEND         !first blank in identifier         !1.7

!declare logical
      LOGICAL     LAND          !value from onland check
      LOGICAL     ONLAND        !Function Subprogram land or sea
      LOGICAL     STANDRD       !indicates standard levels or sig lev
      LOGICAL     ERR
      LOGICAL     ERROR

!declare real
      REAL       A(999)          !TEMP DECODE ELEMENTS ARRAY
      REAL       QCBIT_ARRAY(999)!qc 1bit array for each element
      REAL       RLAT,RLONG      !latitude and longitude in real form
      REAL       VALUES(3000)    !merged array of qcbit and decode elems

      SAVE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!initialize variables,arrays etc                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uaedit.F,v $
     &'//'$ $Date: 30/01/2006 20:25:37$ $Revision: 1$'

      TB17BIT5=0                 !Btye17 Bit 5 for instumentation    !F
      ERROR=.FALSE.
      BLKSIZ=27998               !set blksize of storage dataset
      LAND=.TRUE.
      STANDRD=.TRUE.

      DO K=1,999                 !decode values array
        A(K)=-9999999.           !set to missing
        QCBIT_ARRAY(K)=1.        !qc array set to missing
      ENDDO

      DO K=1,1000                !decriptor array
        DESCR(K)=-9999999        !set to missing
      ENDDO

      DO J=1,3000
        VALUES(J)=-9999999.      !qc and decode array set missing
      ENDDO
      PTR=1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! set day & hour from bulletin heading.  uahead may change them.     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      A(11)=IVALUE(YYGGGG(1:2))     !day from bulletin newformat
      A(12)=IVALUE(YYGGGG(3:4))     !hour from bulletin new format
      TT=TTAAII(1:2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! put current date/time in year,month,day... order for enbufr        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL DATIM(NOW)               !get system
      DO I=0,4                      !date/time
        TOR(1+I)=NOW(8-I)
      ENDDO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! expand, encode & store each report in the bulletin                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      LENG=LEN(REPORT)                                               !g
      IF (LENG.GE.26) THEN          ! throw report away if too short

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! expand the report, deciding which sequence descriptor to use.       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      CALL UAXPAND(REPORT,TT,A,NL,IDENT,DESCR,B17BIT,
     &             PART_TYPE,TYPE,STANDRD,ERR,TB17BIT5,QCBIT_ARRAY)  !g

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Put report in characters at start of string to be stored            !
! (adjusting length if something chopped off start)                  !g
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                     !h
      START=INDEX(REPORT,TYPE)
      IF (START .GT. 0 ) THEN
        LENG=LENG-START+1                                            !g
        MESAGE(1:LENG)=REPORT(START:)
      ELSE
        MESAGE(1:LENG)=REPORT
      ENDIF

      IF (.NOT .ERR) THEN
        ID=IDENT(1:7)                 ! identifier without time on end

        IF (ID(7:7).EQ.CHAR(255)) THEN                             !1.7
          IDEND=INDEX(ID(1:6),' ')                                 !1.7
          IF (IDEND.GT.0) THEN                                     !1.7
            ID(IDEND:)='REJ'                                       !1.7
          ELSE                                                     !1.7
            ID(7:9)='REJ'                                          !1.7
          ENDIF                                                    !1.7
        ENDIF                                                      !1.7

        IF (A(2).EQ.0) THEN
          RETURN
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!We have not checked that the report is correct in being land or      !
!marine. To do this the logical function ONLAND is used. If the value !
!is TRUE then the Lat/Long represent a position over LAND. If the value
!returned is FALSE then the Lat/Long represents a position over sea.  !
!To check whether this agrees with the report BTYE 17 BITS 8 is       !
!looked at. In bit 8 a 0 represents land and 1 sea. If we have a report
!that claims it is marine we can check that its position is infact    !
!over sea.                                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (MOD(ICHAR(IDENT(10:10))/1,2) .EQ. 1) THEN    !1 if marine
          B17_BIT8=1                                     !SET BIT8 VALUE
        ELSE
          B17_BIT8=0
        ENDIF

        IF ((B17_BIT8) .EQ. 1) THEN
          RLAT=A(5)                                     !lat from decode
          RLONG=A(6)                                   !long from decode
          LAND=ONLAND(RLAT,RLONG)                        !

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Now compare the value of LAND and see if it corresponds to the       !
!value of BIT8.                                                       !
!Onland will flag a Lat/Long position as                              !
!land only if there is no sea in the 1 degree square.                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (LAND) THEN                                 !land coords
            IF (B17_BIT8 .EQ. 0) THEN                  !report indicates
              QCBIT_ARRAY(4)=0.                          !land as okay
              QCBIT_ARRAY(5)=0.
            ELSE
              QCBIT_ARRAY(4)=1.                        !sea but lat/long
              QCBIT_ARRAY(5)=1.                          !position over
            ENDIF                                        !land so false
          ELSE
            IF (B17_BIT8 .EQ. 1) THEN                  !report indicates
              QCBIT_ARRAY(4)=0.                          !lat /long okay
              QCBIT_ARRAY(5)=0.
            ELSE
              QCBIT_ARRAY(4)=1.                          !land but lat/
              QCBIT_ARRAY(5)=1.                          !long indicates
            ENDIF                                        !marine
          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!next the qcbit and the elements value array have to be merged together!
!into a single array. That array is then sorted to give a true ascent  !
!profile.                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        A(1)=1                                       !set qcbit ind.

        IF ((A(14) .GT. -99) .OR. (A(18) .GT. -99)) THEN
          CALL UASORT(A,QCBIT_ARRAY,VALUES,STANDRD)
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This section checks that the replication counts are not -ive values.!
!Because the replication count is used in loops in BUFR encode       !
!routines a -ive value is not permissible                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (STANDRD .AND. (A(14) .LE. 0)) THEN    !Check standard report
          ERROR=.TRUE.
        ENDIF

        IF (.NOT. STANDRD) THEN
          IF (A(18) .LT. 0) THEN
            A(18)=0                               !Check sig levels.
            IF (A(19) .LT. 0) THEN
              ERROR=.TRUE.
            ENDIF
          ELSE IF (A(NINT(A(18))*4+19) .LT. 0) THEN                 !2.0
            A(NINT(A(18))*4+19)=0                                   !2.0
          ENDIF
        ENDIF

        IF (STANDRD) THEN
          NL=((A(14)*7)*2)+30          !NUMBER OF ELEMENTS TO ENBUFR
        ELSE
          NL=((A(19+(4*NINT(A(18))))*7)+(A(18)*4)+16)*2+3           !2.0
                                                                !1.6
        ENDIF

        IF (STANDRD) THEN              !for index count of levels
          NUM_LEVS=A(14)
        ELSE
          NUM_LEVS=A((NINT(A(18))*4)+19)                            !2.0
        ENDIF

        IF ((NUM_LEVS.GE.1).AND. (.NOT. ERROR)) THEN
          NDESCR=1                    ! DESCR(1) WAS SET IN UAXPAND
          NOBS=1

          CALL ENBUFR(DESCR,VALUES,NDESCR,NL,NOBS,ID,
     &                TOR,MESAGE(LENG+1:),.FALSE.,L)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! put cccc in section 1 of the bufr message, setting data type too    !
! (displacement as for bufr version 1; change if total length at start)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (CCCC.NE.' ') THEN
            MESAGE(LENG+9:LENG+9)=CHAR(ICCCC/256)
            MESAGE(LENG+10:LENG+10)=CHAR(MOD(ICCCC,256))
          ENDIF

          MESAGE(LENG+13:LENG+13)=CHAR(2) ! data type (upper air)
          READ (CORN,'(I2)') CORNUM !integer cor no. (0 if not cor)

          IHOUR=A(12)                 !hour of data
          MIN=A(13)                   !mins of data (if dropsonde)
          IF (TT.EQ.'UZ' .AND. MIN.GE.0) THEN                        !h
            IDENT(9:9)=CHAR(MIN)
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! start index entry, leaving tor & block/record for tafrep to fill in!
! n.b. the hour here will be changed to one relative to the index hour
! and the bulletin details are for the trailer, they will be replaced!
! by the identifier in the index entry itself.                       !
!For upper air reports, which are chained together in parts, it is  a !
!requirement that the part is available in the trailer for retrieval  !
!purposes. To do this ENTRY(17:17) has bits 7&8 set to indicate the   !
!part. 00 -Part A, 01 - Part B, 10 - Part C and 11 - Part D. Byte 17  !
!of the index entry must also be set to indicate subtype,fixed or     !
!mobile,land or marine. This is set in IDENT(10:10)                   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ENTRY(1:2)=CHAR(IHOUR)//CHAR(0) !TAFREP SETS RELATIVE HOUR
          ENTRY(3:11)=TTAAII(1:4)//CHAR(CORNUM)//CCCC        !IN TRAILER
          ENTRY(12:12)=CHAR(NUM_LEVS)         !LEVELS WITH TEMP OR WIND
          CALL INDLALO(ENTRY,A(5),A(6))                            !1.8

!Bit 6 in Byte17 indicates if report a temp or pilot. If Bit 6 =1 then
!the report is a Temp (Could be land, mobile,fixed etc). So we set bit
!six in the trailer as 0.
          IF (MOD((ICHAR(B17BIT))/4,2) .EQ. 0) THEN                   !A
            PART_TYPE=PART_TYPE+4                                     !A
          ENDIF                                                       !A

!Bit 5 in Byte 17 of the trailer is set to indicate if a pilot report !F
!had a pressure sensor or not. If Bit 5 is set to 0 then the part did !F
!have a sensor. if a part is set to 1 then the part did not have a    !F
!sensor. TB17BIT5 is set in UAPSTD and passed back to this point. The !F
!variable is set to 1 to indicate no pressure sensor and 0 to indicate!F
!part had a pressure sensor.                                          !F
          IF (TB17BIT5 .EQ. 1) THEN                                   !F
            PART_TYPE=PART_TYPE+8                                     !F
          ENDIF                                                       !F

          ENTRY(17:17)=CHAR(PART_TYPE)         !PART A,B,C,D
          IDENT(10:10)=B17BIT          !SUBTYPE,FIXED/MOBILE/LAND/MARINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! put time of data in integer array for tafrep & store report+message!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          DO I=1,5
            DATIME(I)=A(8+I)              ! INTEGER DATE/TIME
          ENDDO

! If minute is missing, set it to 255 (all ones in one byte), not  !1.9
! zero - TAFREP only uses minute to set second byte of index entry.!1.9

          IF (DATIME(5).EQ.-9999999) DATIME(5)=255                 !1.9

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!do not pass report to TAFREP if there is any doubt about the time as!
!TAFREP expects the date/time to be correct                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            IF ((DATIME(1).NE.-9999999).AND.(DATIME(2).GE.1) .AND.
     &        (DATIME(2) .LE. 12) .AND. (DATIME(3) .GE. 1) .AND.
     &        (DATIME(3) .LE. 31) .AND. (DATIME(4) .GE. 0) .AND.
     &        (DATIME(4) .LE. 23)) THEN
                CALL TAFREP(DATIME,ENTRY,MESAGE(:LENG+L),IFT,BLKSIZ,
     &                    IDENT)
            ELSE
              write(6,*)'UAEDIT: report failed time check  ',REPORT !h
            endif
          ENDIF
        ELSE
          WRITE(6,*)'UAEDIT: UAXPAND rejected report',REPORT          !h
        ENDIF
      ENDIF
   99 RETURN
      END
