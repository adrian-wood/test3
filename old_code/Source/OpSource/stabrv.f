      PROGRAM STABRV

!-----------------------------------------------------------------------
!
! PROGRAM       : STABRV
!
! PURPOSE       : To produce an abbreviated station master list to a
!                 browsable data set to meet the OPS operational
!                 requirements.
!
! DESCRIPTION   : This program uses a call to the MDB to look at the
!                 station master list. The stations are then checked
!                 for invalid station data e.g. no reporting practices
!                 and these stations are dropped from the abbreviated
!                 list.
!
! CALLED BY     : None
!
! CALLS         : MDB (STNMAS)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:28$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stabrv.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:28    Sheila Needham  
! $
! Revision 2.1  2001/10/03 15:24:02  usmdb
! Change day 15OCT01 R Hirst
! Enable the writing of records for WMO block 99.
! Also some tidying up.
!
! Revision 2.0  2001/07/03  10:43:58  10:43:58  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/07/04  13:31:28  13:31:28  uspm (Pat McCormack)
! Initial revision
!
! 27/08/96 : Removal of the check on reporting practices. This
!            means all stations will now be included in the
!            abbreviated list.                                        !A
!
! 15/02/96 : Updated to include station name and change the output
!            display
!
! 05/02/96 : CREQ updated to include pressure sensor height along
!            with station height. t12jl
!
! FIRST VERSION DEC95 t12jl
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

      IMPLICIT NONE

      INTEGER PNOBS
      PARAMETER (PNOBS=300)
      INTEGER NOBS                  !no of obs to be recovered
      INTEGER NELEMS                !no of elements to be recovered
      INTEGER ISTAT                 !mdb call return error checking
      INTEGER WMO                   !used in internal write wmo no.
      INTEGER I,J,K                 !used in various loops
      INTEGER F1,F2                 !used to set flags
      INTEGER LOOPA
      INTEGER NUM                   !indicator for surface and/or u/a
      CHARACTER*8   CSUBT           !mdb call parameter
      CHARACTER*118 ABRV            !char of internal write
      CHARACTER*200 CREQ            !mdb call requested information
      CHARACTER*42  CSTR(PNOBS)     !mdb call parameters
      CHARACTER*1   CREP(PNOBS)
      CHARACTER*20  TYPE            !surface/upper air station
      CHARACTER*42  NAME(PNOBS)     !station name
      CHARACTER*42  GETCHR
      CHARACTER*132 HEAD            !revision information

      REAL ARRAY(PNOBS,11)          !set up array(nobs,nelems)
      REAL LATI                     !used in internal write latitude
      REAL LONGI                    !used in internal write longitude
      REAL HGT                      !station level height
      REAL HGTP                     !pressure sensor level
      LOGICAL FLAG,FLAGS,FLAGA,FLAGB!various flag indicators

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stabrv.F,v $
     &'//'$ $Date: 30/01/2006 20:24:28$ $Revision: 1$'

      OPEN(15)                      !open data set for storage

!set-up call parameters

      CSUBT= 'STNMAS  '
      CREQ=' AREA WMO BLOCK XX'//                                 !2.1
     &     ' VERSION LATEST '//
     &     ' ELEMENTS WMO_STN_IX_ID LAT LON PRES_SENS_HT STN_HT'//
     &     ' Q0011_SYN_RPT_TM_ID UAIR_RPT_TM_ID STN_CLS_YR'//
     &     ' Q1223_SYN_RPT_TM_ID STN_AVBY_ID STN_NAME'
!initialize variables
      NELEMS=11                     !no. elements in creq
      NUM=00                        !surface/upper air flag no.

!write header to top of screen and file

      WRITE(6,'(T3,"WMO",T12,"LAT",T21,"LONG",T28,"SENSOR",T37,"STN",
     &T43,"STN",T58,"STATION")')
      WRITE(6,'(1X,T2,"INDEX",T10,"DEG/THS",T19,"DEG/THS",T28,"HT (M)",
     &T36,"HT (M)",T43,"TYPE",T58,"NAME")')
      WRITE(15,'(T3,"WMO",T12,"LAT",T21,"LONG",T28,"SENSOR",T37,"STN",
     &T43,"STN",T58,"STATION")')
      WRITE(15,'(1X,T2,"INDEX",T10,"DEG/THS",T19,"DEG/THS",T28,"HT (M)",
     &T36,"HT (M)",T43,"TYPE",T58,"NAME")')
      K=00
    5 K=K+1                         !increment wmo block no.
      NOBS=PNOBS
      WRITE (CREQ(17:18), '(I2.2)') K !replace new blk no. in creq

!initialize array

      DO I=1,NOBS
        CSTR(I)=' '                 !set any blanks in station
        DO J=1,3                  !master with the value
          ARRAY(I,J)=-999     !-99999 (used for checking)
        END DO
      END DO
      ISTAT=0

!call to mdb

   10 CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEMS,ISTAT,CSTR,CREP)

!check return from mdb

      IF (ISTAT.EQ.16) THEN
        WRITE(6,*)'ERROR FROM MDB'
        GOTO 100
      ELSE IF (ISTAT.EQ.8) THEN
        WRITE(6,*)'NO DATA FOR WMO BLOCK ', K
        WRITE(15,*)'NO DATA FOR WMO BLOCK ',K
        GOTO 15
      ENDIF

!check to see if station is still open. If it is closed ignore entry
!and go on to get more data. Stations that are closed will not
!not appear in the browsable dataset.

      DO LOOPA=1,NOBS
        IF ((ARRAY(LOOPA,8).NE.-9999999) .OR. (ARRAY(LOOPA,10).EQ.
     &   2)) THEN
          GOTO 12                             !skip this station
        ENDIF                                 !as it is closed

!set flags for stations that are only surface or sonde or both
        IF ((ARRAY(LOOPA,6).LT.0000000) .AND. (ARRAY(LOOPA,9) .LT.
     &   000000)) THEN
          F1=0                                !not synop
        ELSE
          F1=1                                !synop station
        ENDIF

        IF (ARRAY(LOOPA,7) .EQ. -9999999) THEN
          F2=0                                !not u/a
        ELSE
          F2=1                                !u/a station
        ENDIF
                                              !station type
        FLAG=((F1.EQ.0) .AND. (F2.EQ.0))      !nothing - error!
        FLAGA=((F1.EQ.0) .AND. (F2.EQ.1))     !u/a only
        FLAGB=((F1.EQ.1) .AND. (F2.EQ.0))     !synop only
        FLAGS=((F1.EQ.1) .AND. (F2.EQ.1))     !u/a and surface

!output the elements we are interested in and flags 1 or 0 to indicate
!surface or sonde or both


        IF (FLAG) THEN                   !no information for station
          NUM=00                         !station may-be closed !A
        ELSEIF (FLAGS) THEN
          NUM=11                         !both u/a and synop
        ELSEIF (FLAGB) THEN
          NUM=10                         !synops only
        ELSEIF (FLAGA) THEN
          NUM=01                         !u/a station
        ENDIF

        WMO=NINT((ARRAY(LOOPA,1)))       !set char for each ob
        LATI=(ARRAY(LOOPA,2))
        LONGI=(ARRAY(LOOPA,3))
        HGT=(ARRAY(LOOPA,5))             !station height
        HGTP=(ARRAY(LOOPA,4))            !pressure sensor hieght
        NAME(LOOPA)=GETCHR(ARRAY(LOOPA,11),CSTR(LOOPA))

!the NUM variable indicates station type but if it is set to 00 then
!there is no reporting practices (r/p's) for that station in the master
!list.Those stations that have no r/p's are not assigned surface and/
!or upper air type but are left blank as they include SREW,NCM stations



!this section produces a browsable dataset for users to look at. The
!dataset contains the wmono. Lat,Long and height as well as the flags
!for upper air/surface reporting.

        IF (NUM .EQ. 10) THEN
          TYPE='SURF'
        ELSEIF (NUM .EQ. 01) THEN
          TYPE='U/AIR'
        ELSEIF (NUM .EQ. 11) THEN
          TYPE='SURF & U/AIR'
        ELSEIF (NUM .EQ. 00) THEN
          TYPE='            '                            !A
        ENDIF

!       write(6,*)WMO,LATI,LONGI,HGTP,HGT,TYPE
        WRITE(ABRV,'(I5.5,1X,F8.3,1X,F8.3,2X,I7,1X,
     &  I7,1X,A12,T57,A42)')WMO,LATI,LONGI,INT(HGTP),INT(HGT),
     &  TYPE,NAME(LOOPA)                          !internal write

        IF (HGT.EQ. -9999999) THEN
          WRITE(ABRV(34:41),'("       ")')
        ENDIF

        IF (HGTP .EQ. -9999999) THEN
          WRITE(ABRV(26:33),'("       ")')
        ENDIF

        WRITE(6,*)ABRV                      !to screen
        WRITE(15,*)ABRV                     !to browsable dataset
 12     CONTINUE
      ENDDO

      IF (ISTAT.EQ.4) THEN        !if there is still more from mdb
        GOTO 10
      ENDIF

!if there is no more from this call to the mdb for the blk in question
!then check if all wmo blks covered if not go back to the beginning to
!increment wmo blk no. and call mdb for new blk.

   15 IF (K.LT.99) GOTO 5        ! was LT.98                        !2.1

  100 CLOSE(15,STATUS='KEEP')                                       !2.1
      STOP                       ! label 100 was on this line       !2.1
      END
