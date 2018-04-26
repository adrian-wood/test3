      SUBROUTINE STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN,       !1.5
     &                   NTYPE, NUMSTNS, LOOKWMO, NPOS, NUMWMO)     !1.5

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! SUBROUTINE   : STARAY                                               
!                                                                     
! PURPOSE      : TO READ THE ABBREVIATED STATION LIST AND STORE       
!                STATION DETAILS IN ARRAYS SUITABLE FOR LOOKING UP.   
!                                                                     
! DESCRIPTION : "STARAY" READS THE ABBREVIATED STATION LIST AND       
!               RETURNS STATION DETAILS IN ARRAYS. AS SOME STATIONS   
!               HAVE SEPARATE ENTRIES FOR SURFACE AND UPPER AIR       
!               REPORTS, AN ADDITIONAL ARRAY HOLDS A LIST OF WMO      
!               NUMBERS WITHOUT DUPLICATES TO BE USED FOR BINARY      
!               SEARCHES.  ANOTHER ARRAY STORES INDEXES FOR           
!               CORRELATING THIS WITH THE FULL LIST.                  
!                                                                     
! USAGE       : CALL STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN,     
!                            NTYPE, NUMSTNS, LOOKWMO, NPOS, NUMWMO    
!                                                                     
! PARAMETERS  : (ALL ARE OUTPUT EXCEPT "NUMSTNS" WHICH IS I&O)        
!                                                                     
!               LISTWMO  (C*4 ARRAY)   WMO NUMBERS OF ALL STATIONS    
!               DEGLAT   (REAL ARRAY)  STATION LATITUDES IN DEGREES   
!               DEGLON   (REAL ARRAY)  STATION LONGITUDES IN DEGREES  
!               HTP      (REAL ARRAY)  STATION PRESSURE SENSOR HEIGHTS
!               HTSTN    (REAL ARRAY)  STATION SURFACE HEIGHTS        
!               NTYPE    (INT. ARRAY)  STATION TYPES (SEE NOTE BELOW) 
!               NUMSTNS  (I*4) INPUT:  MAXIMUM ALLOWED NO. OF STATIONS
!                             OUTPUT:  ACTUAL NO. OF STATIONS IN LIST 
!               LOOKWMO  (C*4 ARRAY)   "LISTWMO" WITHOUT DUPLICATES   
!               NPOS     (I*4 ARRAY)   LOCATIONS IN "LISTWMO" OF FIRST
!                                      ENTRY FOR STATIONS IN "LOOKWMO"
!               NUMWMO   (I*4)         NUMBER OF DIFFERENT WMO NOS.   
!                                                                     
!               NOTE: STATION TYPE IS DEFINED BY THE FOLLOWING CODE - 
!                                                                     
!                      1  NOT SPECIFIED (I.E. BLANK IN LIST)          
!                      2  SURFACE REPORTS ONLY                        
!                      3  UPPER AIR REPORTS ONLY                      
!                      4  SURFACE AND UPPER AIR REPORTS               
!                      0  ANYTHING ELSE                               
!                                                                     
! CALLED BY   : STAPOS                                                
!                                                                     
! CALLS TO    : NONE                                                  
!                                                                     
! DATA SETS   : ABBREVIATED STATION LIST DATA SET ON UNIT 81          
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:30$
! $Source: /data/us0400/mdb/op/lib/source/RCS/staray.F,v $
!                                                                     
! CHANGE RECORD :
!
! AN EARLY VERSION OF THIS ROUTINE WAS BY J LEWTHWAITE AND WAS DATED
! 8 MARCH 1996. IT WAS COMPLETELY RE-WRITTEN BY BRIAN BARWELL IN MAY  
! 1999 TO ALLOW BINARY SEARCHES FOR WMO STATION NUMBERS.
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:30    Sheila Needham  
! $
! Revision 2.1  2003/03/06  09:10:09  09:10:09  usmdb (MetDB account c/o usjh)
! Read Abbreviated station  list filename from environment
! variable METDB_STNABRV - S.Cox
! 
! Revision 2.0  2001/07/03  10:43:58  10:43:58  usmdb (MetDB account c/o usjh)
! Changed pre-processor statement to if (MVS), else...
! Removed READONLY from pre-processor statement. Added copyright
! and modified header - S.Cox
! 
! Revision 1.5  99/06/10  14:45:56  14:45:56  usmdb (Generic MetDB accou
! 21 June 1999, Infoman 63414, Brian Barwell, v(G)=17, v(G)=3.
! New version with binary search of abbreviated station list.
! 
! Revision 1.4  98/01/29  11:26:03  11:26:03  usmdb (Generic MDB account
! Addition of IBM preprocess directive
!
! Revision 1.3  97/11/20  09:55:18  09:55:18  usmdb (Generic MDB account
! Increase size of UpperAir array to cope with additional
! stations.
!
! Revision 1.2  97/08/04  15:44:28  15:44:28  uspm (Pat McCormack)
! Add Y2K check comment
!
! Revision 1.1  1997/07/04 13:32:37  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!----------------------------------------------------------------------
! Declare variables
!----------------------------------------------------------------------

      INTEGER I            ! FOR LOCAL INTERNAL USE                 !1.5
      INTEGER IOS          ! STATUS CODE FROM READ STATEMENT        !1.5
      INTEGER KODE         ! INTERNAL CODE FOR WARNING MESSAGES     !1.5
      INTEGER MAXSTNS      ! MAXIMUM NUMBER OF STATIONS ALLOWED     !1.5
      INTEGER NUMSTNS      ! TOTAL NUMBER IF STATIONS IN LIST       !1.5
      INTEGER NUMWMO       ! NUMBER OF DIFFERENT STATIONS IN LIST   !1.5
      INTEGER LASTWMO      ! LAST WMO STATION NUMBER LOOKED AT      !1.5
      INTEGER LOOKWMO(*)   ! ARRAY OF DIFFERENT WMO STATION NUMBERS !1.5
      INTEGER LISTWMO(*)   ! ARRAY OF ALL WMO STATION NUMBERS       !1.5
      INTEGER NPOS(*)      ! LOCATION IN LISTWMO OF EACH WMO NO.    !1.5
      INTEGER NTYPE(*)     ! STATION TYPES (SEE COMMENTS AT START)  !1.5

      LOGICAL HEADSET      ! TRUE if HEAD set                       !2.1

      REAL DEGLAT(*)       ! LATITUDES OF STATIONS                  !1.5
      REAL DEGLON(*)       ! LONGITUDES OF STATIONS                 !1.5
      REAL HTP(*)          ! PRESSURE SENSOR HEIGHTS OF STATIONS    !1.5
      REAL HTSTN(*)        ! SURFACE HEIGHTS OF STATIONS            !1.5

      CHARACTER*6 BLANKS   ! SIX BLANKS                             !1.5
      CHARACTER*57 REPORT  ! BUFFER TO HOLD ONE STATION'S DETAILS   !1.5
      CHARACTER*132 HEAD   ! FOR REVISION DETAILS

#if ! defined (MVS)
      INTEGER         LEV            !- Length of METDB_STNABRV     !2.1 
      INTEGER         RC             !- Return code                 !2.1
      LOGICAL         FEXIST         !- TRUE if file exists         !2.1
      CHARACTER*200   METDB_STNABRV  !- Station list PATH           !2.1
#endif

!-----------------------------------------------------------------------        
! Data statements
!-----------------------------------------------------------------------        

      DATA BLANKS/'      '/                                         !1.5
      DATA HEADSET/.FALSE./                                         !2.1

!-----------------------------------------------------------------------        
! Revision information
!-----------------------------------------------------------------------        

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: staray.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:24:30$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

!-----------------------------------------------------------------------        
! Initialise variables
!-----------------------------------------------------------------------        

      MAXSTNS = NUMSTNS                                             !1.5
      NUMSTNS = 0                                                   !1.5
      NUMWMO  = 0                                                   !1.5
      LASTWMO = 0                                                   !1.5
      KODE = 0                                                      !1.5

!-----------------------------------------------------------------------        
! Get environment variable METDB_STNABRV and find the length of it.
!-----------------------------------------------------------------------        

#if ! defined (MVS)
      CALL METDB_GETENV("METDB_STNABRV",METDB_STNABRV,RC)           !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'STARAY: ERROR: ENV VAR METDB_STNABRV not set'    !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      LEV=LEN(METDB_STNABRV)                                        !2.1
      DO WHILE (METDB_STNABRV(LEV:LEV).EQ.' ')                      !2.1
        LEV=LEV-1                                                   !2.1
      ENDDO                                                         !2.1
#endif

!----------------------------------------------------------------------
!     OPEN THE ABBREVIATED STATION LIST.
!----------------------------------------------------------------------

#if defined (MVS)
      OPEN(81,FILE='STNABRV',ACTION='READ')                             
#else
      INQUIRE (FILE=METDB_STNABRV(1:LEV),EXIST=FEXIST)              !2.1
      IF (.NOT.FEXIST) THEN                                         !2.1
        WRITE(6,*)'STARAY: ERROR: File ',METDB_STNABRV(1:LEV),      !2.1
     &            ' not found'                                      !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
      OPEN(81,FILE=METDB_STNABRV(1:LEV),IOSTAT=RC)                  !2.1
      IF (RC.NE.0) THEN                                             !2.1
        WRITE(6,*)'STARAY: ERROR: Could not open file ',            !2.1
     &                  METDB_STNABRV(1:LEV)                        !2.1
        RETURN                                                      !2.1
      ENDIF                                                         !2.1
#endif

!----------------------------------------------------------------------
!     LOOP OVER ENTRIES IN STATION LIST, READING AND DECODING RECORDS
!----------------------------------------------------------------------

      DO WHILE (KODE.EQ.0)                                          !1.5
!                                      READ DETAILS FOR NEXT STATION
!
         READ (81, '(A)', IOSTAT=IOS, END=1) REPORT                !1.5
!
!                                               CHECK FOR READ ERROR
         IF (IOS.NE.0) THEN                                         !1.5
            KODE = 2                                                !1.5
!                                CHECK FOR BLANK LINE AT END OF LIST
!
         ELSE IF (REPORT(1:6).EQ.BLANKS) THEN                       !1.5
            KODE = 1                                                !1.5
!                                        CHECK FOR TOO MANY STATIONS
         ELSE IF (NUMSTNS.GE.MAXSTNS) THEN                          !1.5
            KODE = 3                                                !1.5
!                               EXTRACT STATION DETAILS IF GOOD DATA
!
         ELSE IF (REPORT(2:2).GE.'0' .AND. REPORT(2:2).LE.'9') THEN !1.5
            NUMSTNS = NUMSTNS + 1                                   !1.5
!                                                 WMO STATION NUMBER
!                 ("LOOKWMO" IS "LISTWMO" WITHOUT DUPLICATE ENTRIES)
!
            READ (REPORT(2:6),'(I5)') LISTWMO(NUMSTNS)              !1.5
            IF (LISTWMO(NUMSTNS).NE.LASTWMO) THEN                   !1.5
               NUMWMO = NUMWMO + 1                                  !1.5
               LOOKWMO(NUMWMO) = LISTWMO(NUMSTNS)                   !1.5
               NPOS(NUMWMO) = NUMSTNS                               !1.5
               LASTWMO = LISTWMO(NUMSTNS)                           !1.5
            END IF                                                  !1.5
!                                             LATITUDE AND LONGITUDE
!
            READ (REPORT( 8:15),'(F8.3)') DEGLAT(NUMSTNS)           !1.5
            READ (REPORT(17:24),'(F8.3)') DEGLON(NUMSTNS)           !1.5
!
!                                             PRESSURE SENSOR HEIGHT
            I = -9999999                                            !1.5
            IF (REPORT(28:33).NE.BLANKS) READ (REPORT(28:33),'(I6)') I
            HTP(NUMSTNS) = FLOAT(I)                                 !1.5
!                                                     STATION HEIGHT
            I = -9999999                                            !1.5
            IF (REPORT(36:41).NE.BLANKS) READ (REPORT(36:41),'(I6)') I
            HTSTN(NUMSTNS) = FLOAT(I)                               !1.5
!                                                       STATION TYPE
!
            IF (REPORT(43:43).EQ.' ') THEN      ! NOT SPECIFIED     !1.5
               NTYPE(NUMSTNS) = 1                                   !1.5
            ELSE IF (REPORT(43:43).EQ.'U') THEN ! UPPER AIR         !1.5
               NTYPE(NUMSTNS) = 3                                   !1.5
            ELSE IF (REPORT(43:43).EQ.'S') THEN ! SURFACE ...       !1.5
               IF (REPORT(50:50).EQ.'U') THEN   ! ... & UPPER AIR   !1.5
                  NTYPE(NUMSTNS) = 4                                !1.5
               ELSE                             ! ... SURFACE ONLY  !1.5
                  NTYPE(NUMSTNS) = 2                                !1.5
               END IF                                               !1.5
            ELSE                                ! DON'T KNOW        !1.5
               NTYPE(NUMSTNS) = 0                                   !1.5
            END IF                                                  !1.5
         END IF                                                     !1.5
      END DO                                                        !1.5
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF STATIONS
!----------------------------------------------------------------------
!                                                         CLOSE LIST
    1 CONTINUE                                                      !1.5
      CLOSE (81, STATUS='KEEP')
!                                                     OUTPUT MESSAGE
      IF (KODE.LE.1) THEN                                           !1.5
         WRITE (6,'(/T5,A,I8,A)') 'STARAY:', NUMSTNS,               !1.5
     *            ' RECORDS READ FROM THE ABBREVIATED STATION LIST.'!1.5
      ELSE IF (KODE.EQ.2) THEN                                      !1.5
         WRITE (6,'(/T5,A,I6,A)')                                   !1.5
     *            'STARAY:   STATION LIST TRUNCATED AFTER', NUMSTNS,!1.5
     *            ' STATIONS DUE TO I/O ERROR READING LIST.'        !1.5
      ELSE IF (KODE.EQ.3) THEN                                      !1.5
         WRITE (6,'(/T5,A,I6,A)')                                   !1.5
     *            'STARAY:   STATION LIST TRUNCATED AFTER', NUMSTNS,!1.5
     *            ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'          !1.5
      END IF                                                        !1.5
!                                         RETURN TO CALLING PROGRAM
      RETURN
      END
