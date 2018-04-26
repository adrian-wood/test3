SUBROUTINE SETHED(HEADER,RLAT,RLON,IH,IM,ID,IMON,IY, &
ITH,ITM,NAMD,NCOR,CCCC)

!-----------------------------------------------------------------------
!
! PROGRAM       : SETHED IN TFMTRT
!
! PURPOSE       : TO SET UP HEADER FOR A REPORT
!
! DESCRIPTION   : FORMAT
!
!     DATA HEADER/'0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '/
!
! CALLED BY     : TFMTRT IN TFMTRT
!
! CALLS         : NIL
!
! PARAMETERS    : (1)HEADER     44 BYTE OUTPUT HEADER
!                 (2)RLAT       REAL LATITUDE
!                 (3)RLON       REAL LONGITUDE
!                 (4)IH         DATE
!                 (5)IM           TIME
!                 (6)ID             OF
!                 (7)IMON             OBSERVATION
!                 (8)IY
!                 (9)ITH        HOUR OF RECEIPT
!                (10)ITMMINUTE OF RECEIPT
!                (11)NAMD       AMEND NUMBER
!                (12)NCOR       COR NUMBER
!                (13)CCCC       COLLECTING CENTRE
!
!Y2K  01.07.1997  SETHED is Year 2000 compliant.
!Y2K                     Routine contains date management.
!
! REVISION INFO :
!
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         18/11/2010 11:50:20    Stan Kellett
!       copyright changed to 2010, removed simple data statement to set on
!       declaration. Removed some old revision comments.
!  4    MetDB_Refresh 1.3         11/11/2010 10:56:48    Richard Weedon  rework
!        after peer review
!  3    MetDB_Refresh 1.2         02/11/2010 14:17:30    Richard Weedon
!       removed old change information
!  2    MetDB_Refresh 1.1         26/10/2010 12:10:42    Richard Weedon  update
!        to intent statements
!  1    MetDB_Refresh 1.0         26/10/2010 11:49:45    Richard Weedon
!       Initial port to f95. Set to f95 standard
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

CHARACTER(LEN=*),INTENT(INOUT)    ::   HEADER
CHARACTER(LEN=4),INTENT(IN)       ::   CCCC
REAL,INTENT(IN)                   ::   RLAT
REAL,INTENT(IN)                   ::   RLON
REAL                              ::   RMDI=-9999999.0
INTEGER,INTENT(IN)                ::   IH
INTEGER,INTENT(IN)                ::   IM
INTEGER,INTENT(IN)                ::   ID
INTEGER,INTENT(IN)                ::   IMON
INTEGER,INTENT(IN)                ::   IY
INTEGER,INTENT(IN)                ::   ITH
INTEGER,INTENT(IN)                ::   ITM
INTEGER,INTENT(IN)                ::   NAMD
INTEGER,INTENT(IN)                ::   NCOR

IF(RLAT == RMDI)THEN
  HEADER(16:21)=' '
  HEADER(23:29)=' '
ELSE
  IF(RLAT > 0)THEN
    HEADER(21:21)='N'
  ELSE
    HEADER(21:21)='S'
  END IF
  WRITE(HEADER(16:20),'(F5.2)')ABS(RLAT)
  IF(RLON > 0)THEN
    HEADER(29:29)='E'
  ELSE
    HEADER(29:29)='W'
  END IF
  WRITE(HEADER(23:28),'(F6.2)')ABS(RLON)
END IF
IF ((ID == RMDI).OR.(IMON == RMDI).OR.(IY == RMDI)) THEN   
  WRITE(HEADER(1:6),2) IH,IM                              
  HEADER(7:14) = '        '                               
ELSE
  WRITE(HEADER(1:14),1)IH,IM,ID,IMON,MOD(IY,100)          
END IF
1     FORMAT(2I2.2,'Z ',2(I2.2,'/'),I2.2)
2     FORMAT(2I2.2,'Z ')
WRITE(HEADER(36:39),'(2I2.2)')ITH,ITM
HEADER(31:34)=CCCC
WRITE(HEADER(41:43),'(I1,1X,I1)')NAMD,NCOR

RETURN
END SUBROUTINE SETHED
