      PROGRAM OBNMEU

!-----------------------------------------------------------------------
!
! program       : OBNMEU
!
! purpose       : Routine called by REXX script OBNMEU on
!               : MDB.REXX.EXEC to calculate the percentage of European
!               : SYNOPS receieved within a specific hour. Expected
!               : totals are taken from list of stations used to plot
!               : European Chart.
!               : The list of obs is broken down into areas each with a
!               : maximum number of 50 reports. If the number of
!               : stations in an area exceeds 50 then they are moved to
!               : overflow area.
!
! called by     : OBNMEU on MDB.REXX.EXEC
!
! arguments     : picked up by assembler routine REXXVAR
!
! Date          : char*(*) (ip) : Code name to get Code value for
! Time          : char*(*) (ip) : Code name to get Code value for
! ExpectedObs   : integer  (ip) : Length of NAME (variable)
! ActualObs     : integer  (op) : Code value for Code name
! Percentage    : integer  (op) : Code value for Code name
!
!Y2K  18.02.1999  OBNMEU is year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/obnmeu.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:48    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:03  usmdb
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.1  99/03/11  15:00:52  15:00:52  usmdb (Generic MDB account)
! Initial revision
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

      INTEGER IOBS,IELS,MXAREA

      PARAMETER      (IOBS=50)
      PARAMETER      (IELS=2)
      PARAMETER      (MXAREA=13)

      CHARACTER*8    CSUBT
      CHARACTER*800  TREQ
      CHARACTER*800  CREQ
      REAL           ARRAY(IOBS,IELS)
      INTEGER        NOBS
      INTEGER        NELEM
      INTEGER        ISTAT
      INTEGER        CURRENT(8)
      INTEGER        CURRENTDAY
      INTEGER        REQYEAR,REQMONTH,REQDAY
      REAL           REALPERCENTAGEOBS
      INTEGER        INTACTUALOBS
      INTEGER        IRC
      CHARACTER*1    CSTR(IOBS)
      CHARACTER*5    TIME
      CHARACTER*8    REQDATE
      CHARACTER*1    DATE
      CHARACTER*30   SUBMITTYPE
      CHARACTER*1    CREP(IOBS)
      CHARACTER*132  HEAD
      CHARACTER*80   STNLNE         !line of FT14 dataset
      CHARACTER*300  STNLS(MXAREA)  !char string for stns in each area
      CHARACTER*20   RAREA(MXAREA)  !name of each area

      INTEGER        INTTIME
      INTEGER        I
      INTEGER        IL             !Length of creq
      INTEGER        KSTN           !TOTAL OBS REQUIRED
      INTEGER        KSTNR(MXAREA)  !TOTAL OBS REQUIRED per area
      INTEGER        TSTN           !TOTAL OBS REceived
      INTEGER        TSTNR(MXAREA)  !TOTAL OBS REceived per area
      INTEGER        PCSTN          !Percentage of TOTAL OBS REceived
      INTEGER        PCSTNR(MXAREA) ! " "    per area
      INTEGER        ENDLSR(MXAREA) ! End char. no. of stations list
      INTEGER        READ14         ! Status of reading FT14
      INTEGER        STAT14         ! Status of opening FT14
      INTEGER        STNNUM         ! Station number
      INTEGER        IVALUE         ! integer function
      INTEGER        AREA           ! loop variable for each area

!-----------------------------------------------------------------------
! RCS information
!-----------------------------------------------------------------------

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/obnmeu.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:23:48$ '

!-----------------------------------------------------------------------
! Read in expected obs from file. File allocated in calling REXX
!-----------------------------------------------------------------------

      OPEN(14,IOSTAT=STAT14)

      IF(STAT14.EQ.0)THEN

!-----------------------------------------------------------------------
! Setup decriptions of output areas
!-----------------------------------------------------------------------

        RAREA(01)=' WMO Blk  01        '
        RAREA(02)=' WMO Blk  02        '
        RAREA(03)=' WMO Blk  03        '
        RAREA(04)=' WMO Blk  04        '
        RAREA(05)=' WMO Blk  06        '
        RAREA(06)=' WMO Blk  07        '
        RAREA(07)=' WMO Blk  08        '
        RAREA(08)=' WMO Blks 09,10,11  '
        RAREA(09)=' WMO Blks 12,13,14  '
        RAREA(10)=' WMO Blk  16-19     '
        RAREA(11)=' WMO Blks 20-30     '
        RAREA(12)=' North Africa       '
        RAREA(13)=' overflows          '

!-----------------------------------------------------------------------
! Initialise area variables
!-----------------------------------------------------------------------

        READ14= 0
        KSTN= 0
        TSTN= 0
        PCSTN= 0
        DO AREA=1,MXAREA
          KSTNR(AREA)=0
          TSTNR(AREA)=0
          PCSTNR(AREA)=0
          STNLS(AREA)=' '
        ENDDO

!  READ IN LIST OF STATIONS REQUIRED

        DO WHILE (READ14.EQ.0.AND.KSTN.LT.830)

          READ(14,120,IOSTAT=READ14)STNLNE
 120               FORMAT(A80)
          IF(STNLNE(1:5).EQ.'     ')THEN
            STNNUM=IVALUE(STNLNE(6:10))
            IF(STNNUM.GT.0.AND.STNNUM.LT.64000)THEN
              KSTN=KSTN+1
              IF(STNNUM.GT.0.AND.STNNUM.LT.02000.AND.
     &           KSTNR(01).LT.51)THEN
                KSTNR(1)=KSTNR(1)+1
                ENDLSR(1)=KSTNR(1)*6
                STNLS(1)(ENDLSR(1)-5:ENDLSR(1))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.02000.AND.STNNUM.LT.03000.AND.
     &           KSTNR(02).LT.51)THEN
                KSTNR(2)=KSTNR(2)+1
                ENDLSR(2)=KSTNR(2)*6
                STNLS(2)(ENDLSR(2)-5:ENDLSR(2))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.03000.AND.STNNUM.LT.04000.AND.
     &           KSTNR(03).LT.51)THEN
                KSTNR(3)=KSTNR(3)+1
                ENDLSR(3)=KSTNR(3)*6
                STNLS(3)(ENDLSR(3)-5:ENDLSR(3))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.04000.AND.STNNUM.LT.06000.AND.
     &           KSTNR(04).LT.51)THEN
                KSTNR(4)=KSTNR(4)+1
                ENDLSR(4)=KSTNR(4)*6
                STNLS(4)(ENDLSR(4)-5:ENDLSR(4))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.06000.AND.STNNUM.LT.07000.AND.
     &           KSTNR(05).LT.51)THEN
                KSTNR(5)=KSTNR(5)+1
                ENDLSR(5)=KSTNR(5)*6
                STNLS(5)(ENDLSR(5)-5:ENDLSR(5))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.07000.AND.STNNUM.LT.08000.AND.
     &           KSTNR(06).LT.51)THEN
                KSTNR(6)=KSTNR(6)+1
                ENDLSR(6)=KSTNR(6)*6
                STNLS(6)(ENDLSR(6)-5:ENDLSR(6))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.08000.AND.STNNUM.LT.09000.AND.
     &           KSTNR(07).LT.51)THEN
                KSTNR(7)=KSTNR(7)+1
                ENDLSR(7)=KSTNR(7)*6
                STNLS(7)(ENDLSR(7)-5:ENDLSR(7))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.09000.AND.STNNUM.LT.12000.AND.
     &           KSTNR(08).LT.51)THEN
                KSTNR(8)=KSTNR(8)+1
                ENDLSR(8)=KSTNR(8)*6
                STNLS(8)(ENDLSR(8)-5:ENDLSR(8))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.12000.AND.STNNUM.LT.16000.AND.
     &           KSTNR(08).LT.51)THEN
                KSTNR(9)=KSTNR(9)+1
                ENDLSR(9)=KSTNR(9)*6
                STNLS(9)(ENDLSR(9)-5:ENDLSR(9))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.16000.AND.STNNUM.LT.20000.AND.
     &           KSTNR(10).LT.51)THEN
                KSTNR(10)=KSTNR(10)+1
                ENDLSR(10)=KSTNR(10)*6
                STNLS(10)(ENDLSR(10)-5:ENDLSR(10))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.20000.AND.STNNUM.LT.30000.AND.
     &           KSTNR(11).LT.51)THEN
                KSTNR(11)=KSTNR(11)+1
                ENDLSR(11)=KSTNR(11)*6
                STNLS(11)(ENDLSR(11)-5:ENDLSR(11))=STNLNE(6:11)
              ELSEIF(STNNUM.GE.60000.AND.STNNUM.LT.64000.AND.
     &           KSTNR(12).LT.51)THEN
                KSTNR(12)=KSTNR(12)+1
                ENDLSR(12)=KSTNR(12)*6
                STNLS(12)(ENDLSR(12)-5:ENDLSR(12))=STNLNE(6:11)
              ELSE
                KSTNR(13)=KSTNR(13)+1
                ENDLSR(13)=KSTNR(13)*6
                STNLS(13)(ENDLSR(13)-5:ENDLSR(13))=STNLNE(6:11)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        CLOSE(14)

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

        Csubt          = 'LNDSYN  '
        Nobs           = Iobs
        Nelem          = Iels
        IntActualObs   = 0
        Istat          = 0

        CALL REXXVAR('F','DATE',4,Date,1,'N',Irc)
        CALL REXXVAR('F','TIME',4,Time,5,'N',Irc)
        CALL REXXVAR('F','SUBMITTYPE',10,SubmitType,30,'N',Irc)

!       Date = 'T'                                  ! TEST INPUT LINE
!       Time = '0000Z'                              ! TEST INPUT LINE
!       SubmitType = 'EXTRACT LATEST HOUR'          ! TEST INPUT LINE

!-----------------------------------------------------------------------
! Construct date of request
!-----------------------------------------------------------------------

        READ(Time(1:2),'(I2)')IntTime

        CALL DATIM(Current)
        CALL DATE31(Current(6),Current(7),Current(8),CurrentDay)

        IF (SubmitType(1:19).EQ.'EXTRACT LATEST HOUR') THEN
          ReqDay   = Current(6)
          ReqMonth = Current(7)
          ReqYear  = Current(8)
          WRITE(Time(1:2),'(I2.2)')Current(5)
        ELSE

          IF (Date(1:1).EQ.'Y') THEN
            CALL DATE13(CurrentDay-1,ReqDay,ReqMonth,ReqYear)
          ELSE
            ReqDay   = Current(6)
            ReqMonth = Current(7)
            ReqYear  = Current(8)

            IF (IntTime.GT.Current(5)) THEN
              WRITE(6,*)'<pre><b>'
              WRITE(6,*)'Invalid request - request date/time greater ',
     &                'than current date/time'
              WRITE(6,*)'</b></pre>'
              STOP
            ENDIF

          ENDIF
        ENDIF

        WRITE(ReqDate(1:4),'(I4.4)')ReqYear
        WRITE(ReqDate(5:6),'(I2.2)')ReqMonth
        WRITE(ReqDate(7:8),'(I2.2)')ReqDay

!-----------------------------------------------------------------------
! Loop through the areas
!-----------------------------------------------------------------------

        WRITE(6,*)'<pre>'
        DO AREA=1,MXAREA
          IF(KSTNR(AREA).GT.0)THEN
            NOBS=KSTNR(AREA)
            TREQ=' '

!-----------------------------------------------------------------------
! Construct MDB request string.
!-----------------------------------------------------------------------

            TREQ  = ' PLATFORM ' // STNLS(AREA)(1:ENDLSR(AREA)) //
     &        'START TIME ' // REQDATE // '/' // TIME(1:5) // ' ' //
     &        'END TIME ' // REQDATE // '/' // TIME(1:5) // ' ' //
     &        'ELEMENTS WMO_BLCK_NMBR WMO_STTN_NMBR '

            CALL DELSPCE(CREQ,IL,TREQ)

!-----------------------------------------------------------------------
! Call The MetDB. Increment the actual number of obs returned.
!-----------------------------------------------------------------------

  1         CALL MDB(CSUBT,CREQ(1:IL),ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

            TSTN = TSTN + NOBS

            DO I=1,NOBS
              STNNUM=NINT(ARRAY(I,1)*1000)+NINT(ARRAY(I,2))
              IF(STNNUM.GT.0.AND.STNNUM.LT.02000)THEN
                TSTNR(1)=TSTNR(1)+1
              ELSEIF(STNNUM.GE.02000.AND.STNNUM.LT.03000)THEN
                TSTNR(2)=TSTNR(2)+1
              ELSEIF(STNNUM.GE.03000.AND.STNNUM.LT.04000)THEN
                TSTNR(3)=TSTNR(3)+1
              ELSEIF(STNNUM.GE.04000.AND.STNNUM.LT.06000)THEN
                TSTNR(4)=TSTNR(4)+1
              ELSEIF(STNNUM.GE.06000.AND.STNNUM.LT.07000)THEN
                TSTNR(5)=TSTNR(5)+1
              ELSEIF(STNNUM.GE.07000.AND.STNNUM.LT.08000)THEN
                TSTNR(6)=TSTNR(6)+1
              ELSEIF(STNNUM.GE.08000.AND.STNNUM.LT.09000)THEN
                TSTNR(7)=TSTNR(7)+1
              ELSEIF(STNNUM.GE.09000.AND.STNNUM.LT.12000)THEN
                TSTNR(8)=TSTNR(8)+1
              ELSEIF(STNNUM.GE.12000.AND.STNNUM.LT.16000)THEN
                TSTNR(9)=TSTNR(9)+1
              ELSEIF(STNNUM.GE.16000.AND.STNNUM.LT.20000)THEN
                TSTNR(10)=TSTNR(10)+1
              ELSEIF(STNNUM.GE.20000.AND.STNNUM.LT.30000)THEN
                TSTNR(11)=TSTNR(11)+1
              ELSEIF(STNNUM.GE.60000.AND.STNNUM.LT.64000)THEN
                TSTNR(12)=TSTNR(12)+1
              ELSE
                TSTNR(13)=TSTNR(13)+1
              ENDIF
            ENDDO

            IF (ISTAT.EQ.4) GOTO 1
          ENDIF

        ENDDO

!-----------------------------------------------------------------------
! Output information for display by OBNMUE REXX script.
!-----------------------------------------------------------------------

        WRITE(6,*)'<B>'
        REALPERCENTAGEOBS = FLOAT(TSTN)/FLOAT(KSTN)*100.0

        WRITE(6,*)
     &' All Land Obs       Expected obs  Actual obs Percentage obs '
        WRITE(6,'(25X,I3,10X,I3,10X,F6.2)')
     & KSTN,TSTN,RealPercentageObs
        WRITE(6,*)' '

        WRITE(6,*)
     &' Area               Expected obs  Actual obs Percentage obs '
        WRITE(6,*)'</B>'
!
! Loop through areas outputing ob counts and percentage if any stations
! expected.
!
        DO AREA=1,MXAREA

          IF (KSTNR(AREA).LE.0) THEN
            RealPercentageObs = 100.0
          ELSE
            RealPercentageObs =
     &           FLOAT(TSTNR(AREA))/FLOAT(KSTNR(AREA))*100.0
C         'AAAA AAAA AAAA AAAA XXXX IIIXXXX XXXX IIIXXXX XXXX FFFFFF   '
            WRITE(6,'(A20,5X,I3,10X,I3,10X,F6.2)')
     &     RAREA(AREA),KSTNR(AREA),TSTNR(AREA),REALPERCENTAGEOBS        OBS
          ENDIF

        ENDDO

!
! Output a message if overflow area is used because an area has more
! than 50 stations
!
        IF(KSTNR(13).GT.0)THEN
          WRITE(6,*)'CONTACT MetDB Team - '
     &              ,' program OBNMEU needs updating'
        ENDIF

        WRITE(6,*)'</pre>'
      ELSE
!
! Output a message when non-zero return code from opening station
!  details dataset.
!
        WRITE(6,*)'<B>'
        WRITE(6,*)'<P>ERROR opening station details dataset.</P>'
        WRITE(6,*)'</B>'
      ENDIF
!
! Ask users to click on reload button to update display
!
      WRITE(6,*)'<P>Click "Reload" to update totals for this hour</P>'

      STOP
      END
