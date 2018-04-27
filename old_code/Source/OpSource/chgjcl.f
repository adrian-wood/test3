      PROGRAM CHGJCL

      IMPLICIT  NONE

!----------------------------------------------------------------------
!
! PROGRAM       : CHGJCL
!
! PURPOSE       : TO CHANGE LINE(S) IN METDB CHASER JOB JCL TO MAKE
!                 FT12 DATASET HAVE A DATE IN NAME.
!
! CALLED BY     : NONE
!
! CALLS         : DATIM
!
! PARAMETERS    : NONE
!
!Y2K  03.09.1998  CHGJCL IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chgjcl.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:40    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:29  usmdb
! Corrected revision info, removed unused variables.
! Added copyright and modified header - S.Cox
!
! Revision 1.2  98/09/30  09:50:56  09:50:56  usmdb (Generic MDB account)
! Added processing of FT14 line in JCL for daily chaser dataset. 
! Removed processing for SDB submission. 
! v(G)=11, ev(G)= 1
! 
! Revision 1.1  98/09/08  16:11:48  16:11:48  usmdb (Generic MDB account
! Initial revision
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

! DECLARATIONS:

! GLOBAL VARIABLES:

      INTEGER      TIME(8)     !  CURRENT DATE AND TIME INFORMATION

! LOCAL PARAMETERS:

C     NONE

! LOCAL SCALARS:

      CHARACTER LINE*80     !  A LINE OF DATASET
      INTEGER   LINENO      !  LINE NUMBER
      INTEGER   REJPOS      !  POSITION OF REJECT ON LINE
      INTEGER   STAT95      !  RETURN CODE FROM OPENING FT95
      INTEGER   STAT96      !  RETURN CODE FROM OPENING FT96
      INTEGER   STATRD      !  RETURN CODE FROM READING FT95
      INTEGER   STATWR      !  RETURN CODE FROM WRITING TO FT95
      INTEGER   STATTY      !  RETURN CODE FROM READING FT05
      INTEGER   YR          !  LAST TWO DIGITS OF YEAR
      CHARACTER*132  HEAD

! END OF DECLARATIONS:

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chgjcl.F,v $
     &'//'$Date: 30/01/2006 20:21:40$ $Revision: 1$'                                   !2.0

!  SET CONTROL VARIABLES

      LINENO = 0

!  INITIALISE STATUS VARIABLES FOR READS AND WRITES

      STAT95 = 0
      STAT96 = 0
      STATRD = 0
      STATWR = 0
      STATTY = 0

!  READ CURRENT SYSTEM DATE AND TIME

      CALL DATIM(TIME)

!  SET YR TO LAST TWO DIGITS OF YEAR

      YR = MOD(TIME(8),100)

!  OPEN DATASETS CONTAINING JCL TO BE CHANGED

      OPEN(95,IOSTAT=STAT95,ERR=777)
      OPEN(96,IOSTAT=STAT96,ERR=777)

!  ONLY PROCESS JCL IF DATASETS CAN BE OPENED

 777  IF(STAT95.EQ.0.AND.STAT96.EQ.0)THEN

!  READ IN DATASET LINES

        DO WHILE (STATRD.EQ.0)

          READ(95,120,IOSTAT=STATRD,END=888)LINE
 120      FORMAT(A80)

! FIND LINE IN JCL WITH 'FT12F001' TO CHANGE OUTPUT DATASET NAME

!   //FT12F001 DD DSN=MCC3.DBMONOUT.CSddmmyy,                       !1.2
!                            0123456789012345,

          IF(INDEX(LINE,'FT12F001').NE.0.AND.STATRD.EQ.0)THEN
             REJPOS=INDEX(LINE,'MONOUT')
            IF(REJPOS.NE.0)THEN
              WRITE(6,120)LINE
              WRITE(LINE(REJPOS+09:REJPOS+10),'(I2.2)')TIME(6)
              WRITE(LINE(REJPOS+11:REJPOS+12),'(I2.2)')TIME(7)
              WRITE(LINE(REJPOS+13:REJPOS+14),'(I2.2)')YR
              LINE(REJPOS+15:REJPOS+15) = ','
              WRITE(6,120)LINE
            ENDIF
          ENDIF
                                                                    !1.2
! FIND LINE IN JCL WITH 'FT14F001' TO CHANGE OUTPUT DATASET NAME    !1.2
                                                                    !1.2
!   //FT14F001 DD DSN=MCC3.DBMONOUT.DYddmmyy,                       !1.2
!                            0123456789012345,                      !1.2
                                                                    !1.2
          IF(INDEX(LINE,'FT14F001').NE.0.AND.STATRD.EQ.0)THEN       !1.2
             REJPOS=INDEX(LINE,'MONOUT')                            !1.2
            IF(REJPOS.NE.0)THEN                                     !1.2
              WRITE(6,120)LINE                                      !1.2
              WRITE(LINE(REJPOS+09:REJPOS+10),'(I2.2)')TIME(6)      !1.2
              WRITE(LINE(REJPOS+11:REJPOS+12),'(I2.2)')TIME(7)      !1.2
              WRITE(LINE(REJPOS+13:REJPOS+14),'(I2.2)')YR           !1.2
              LINE(REJPOS+15:REJPOS+15) = ','                       !1.2
              WRITE(6,120)LINE                                      !1.2
            ENDIF                                                   !1.2
          ENDIF                                                     !1.2

! WRITE OUT LINE TO FT96 DATASET

          WRITE(96,120,IOSTAT=STATWR)LINE
          IF(STATWR.NE.0)THEN
            WRITE(6,89)STATWR
  89        FORMAT(' ERROR WRITING LINE OUT',I4)
          ENDIF

 888      CONTINUE

        END DO

!  CLOSE DATASET

        CLOSE(95)
        CLOSE(96)
      ELSE
        WRITE(6,93)STAT95,STAT96
  93    FORMAT(1X,' ERROR ON OPEN STATUS FT95,FT96  STATUS = ',2I8)
        CALL SYSRCX (2120)
      ENDIF

      STOP
      END
