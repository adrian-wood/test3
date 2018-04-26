SUBROUTINE STNRET(WMOLST,ICAOLST,DCNNLST,RAINLST,                    &
                  IDTYPE,POS,ISTAT,BLK,STN,ICAO,                     &
                  LAT1,LAT2,LATP,LONG1,LONG2,LONGP,HT1,HT2,          &
                  STATION,COUNTRY,REGION,DCNN,RAIN,                  &
                  OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,   &
                  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,   &
                  UA,SYN,NCM,NUM,DAY,                                &
                  SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNRET IN STNRT
!
! PURPOSE       : CALL STNINDX TO FIND RECORD NUMBER FOR STATION
!                 DETAILS AND STNFIN TO READ RECORD.
!
! CALLED BY     : STNRT
!
! CALLS         : STNINDX & STNFIN IN STNRT
!
! ARGUMENTS     : (1-6) ARGUMENTS FOR STNINDX
!                 (7) RETURN CODE
!                 (8-51) ARGUMENTS FOR STNFIN
!
! REVISION INFO :
!
!
! $Workfile: stnret.F90$ $Folder: OpSource$
! $Revision: 10$ $Date: 20/06/2011 07:52:05$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         20/06/2011 07:52:05    Sheila Needham
!       Correct deallocate statements!
!  9    MetDB_Refresh 1.8         20/06/2011 07:08:09    Sheila Needham
!       Correct type of FIRST
!  8    MetDB_Refresh 1.7         19/06/2011 10:42:43    Sheila Needham
!       Re-write to optimise I/O performance
!  7    MetDB_Refresh 1.6         09/02/2011 17:20:29    Sheila Needham  SAVE
!       variables
!  6    MetDB_Refresh 1.5         30/11/2010 14:22:02    Stan Kellett    
!  5    MetDB_Refresh 1.4         22/11/2010 14:31:11    Stan Kellett
!       corrected preprocessor statement
!  4    MetDB_Refresh 1.3         19/11/2010 15:52:13    Stan Kellett    
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:10:31    Rosemary Lavery remove
!        old revision info
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE stnindx_mod
USE stnfin_mod
USE stnmas_mod    ! for TYPE(stnmas_details) definition

IMPLICIT NONE

! Interface Arguments

INTEGER,            INTENT(INOUT)  :: WMOLST(50)
CHARACTER (LEN=4),  INTENT(INOUT)  :: ICAOLST(50)
INTEGER,            INTENT(INOUT)  :: DCNNLST(50)
INTEGER,            INTENT(INOUT)  :: RAINLST(50)
INTEGER,            INTENT(INOUT)  :: IDTYPE
CHARACTER (LEN=1),  INTENT(INOUT)  :: POS
INTEGER,            INTENT(INOUT)  :: ISTAT
INTEGER,            INTENT(INOUT)  :: BLK
INTEGER,            INTENT(INOUT)  :: STN
CHARACTER (LEN=4),  INTENT(INOUT)  :: ICAO
INTEGER,            INTENT(INOUT)  :: LAT1
INTEGER,            INTENT(INOUT)  :: LAT2
CHARACTER (LEN=1),  INTENT(INOUT)  :: LATP
INTEGER,            INTENT(INOUT)  :: LONG1
INTEGER,            INTENT(INOUT)  :: LONG2
CHARACTER (LEN=1),  INTENT(INOUT)  :: LONGP
INTEGER,            INTENT(INOUT)  :: HT1
INTEGER,            INTENT(INOUT)  :: HT2
CHARACTER (LEN=42), INTENT(INOUT)  :: STATION
CHARACTER (LEN=32), INTENT(INOUT)  :: COUNTRY
INTEGER,            INTENT(INOUT)  :: REGION
INTEGER,            INTENT(INOUT)  :: DCNN
INTEGER,            INTENT(INOUT)  :: RAIN
INTEGER,            INTENT(INOUT)  :: OYEAR
INTEGER,            INTENT(INOUT)  :: OMONTH
INTEGER,            INTENT(INOUT)  :: ODAY
INTEGER,            INTENT(INOUT)  :: OHOUR
INTEGER,            INTENT(INOUT)  :: CYEAR
INTEGER,            INTENT(INOUT)  :: CMONTH
INTEGER,            INTENT(INOUT)  :: CDAY
INTEGER,            INTENT(INOUT)  :: CHOUR
CHARACTER (LEN=24), INTENT(INOUT)  :: FLAGS
INTEGER,            INTENT(INOUT)  :: CLASS
INTEGER,            INTENT(INOUT)  :: HAW00
INTEGER,            INTENT(INOUT)  :: HAW12
INTEGER,            INTENT(INOUT)  :: MAP
INTEGER,            INTENT(INOUT)  :: RUN1
INTEGER,            INTENT(INOUT)  :: RUN2
INTEGER,            INTENT(INOUT)  :: OREC
INTEGER,            INTENT(INOUT)  :: NREC
CHARACTER (LEN=4),  INTENT(INOUT)  :: UA
CHARACTER (LEN=13), INTENT(INOUT)  :: SYN
CHARACTER (LEN=23), INTENT(INOUT)  :: NCM
INTEGER,            INTENT(INOUT)  :: NUM
CHARACTER (LEN=9),  INTENT(INOUT)  :: DAY(10)
CHARACTER (LEN=24), INTENT(INOUT)  :: SYNTIM(10)
INTEGER,            INTENT(INOUT)  :: NCMTIM1(10)
INTEGER,            INTENT(INOUT)  :: NCMTIM2(10)
INTEGER,            INTENT(INOUT)  :: SQUARE
CHARACTER (LEN=6),  INTENT(INOUT)  :: CHASER
INTEGER,            INTENT(INOUT)  :: RECNO


! Local Variables

INTEGER                :: IAGAIN
INTEGER                :: INDREC2
INTEGER                :: INDREC(1000)
INTEGER                :: ORDERED(1000)
INTEGER                :: INDWMO(1000)
INTEGER                :: IJK
INTEGER                :: IP
INTEGER                :: COUNT
INTEGER                :: NFOUND
LOGICAL                :: FIRST=.TRUE.
TYPE(stnmas_details),ALLOCATABLE   :: TABLE(:)

SAVE

! RESET IAGAIN AND IJK IF ROUTINE REENTERED WITH NEW REQUEST

IF (ISTAT == 0) THEN
  IAGAIN=0
  IJK=1
END IF

! Set up table for a new request

IF_NEWREQ: &
IF (IAGAIN == 0) THEN
  IF (FIRST) THEN     ! only open on first request
#if defined (MVS)
    OPEN(88,ACCESS='DIRECT',ACTION='READ',  &
      FORM='FORMATTED',RECL=692)
    OPEN(89,FILE='DD:STNIND',ACCESS='DIRECT',ACTION='READ',  &
      FORM='FORMATTED',RECL=692)
#else
    OPEN(88,ACCESS='DIRECT',FORM='FORMATTED',RECL=692)
    OPEN(89,FILE='DD:STNIND',ACCESS='DIRECT',  &
            FORM='FORMATTED',RECL=692)
#endif
    FIRST=.FALSE.
  END IF

  IAGAIN=1

! IF NEW REQUEST, CALL INDEX SUBROUTINE TO FIND STATION RECORD NUMBERS

  CALL STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST,  &
               IDTYPE,POS,ISTAT,INDREC,INDWMO,NFOUND)
  IF (INDREC(1) <= 0) THEN
    ISTAT=16
    GOTO 999
  END IF

! Get a list of ordered record numbers so that STNFIN reads records
! in order, to optimise I/O performance
  CALL ORDER(INDREC,NFOUND,ORDERED)

! allocate space for a table of Stationmaster records for this request.
! Deallocate from any previous request as it might be a different size.

  IF (ALLOCATED(TABLE))DEALLOCATE(TABLE)
  ALLOCATE(TABLE(NFOUND))

! load table of records for all stations in this request
  DO COUNT=1,NFOUND
    IP = ORDERED(COUNT)
    INDREC2 = INDREC(IP)
    CALL STNFIN(INDREC2,TABLE(IP))
  END DO

END IF IF_NEWREQ

IF_GETDTL: &
IF (ISTAT <= 4) THEN
  IF (IJK > NFOUND) THEN
    ISTAT=16
    GOTO 999
  END IF
! Assign variables from table
  BLK=TABLE(IJK)%BLK
  STN=TABLE(IJK)%STN
  ICAO=TABLE(IJK)%ICAO
  LAT1=TABLE(IJK)%LAT1
  LAT2=TABLE(IJK)%LAT2
  LATP=TABLE(IJK)%LATP
  LONG1=TABLE(IJK)%LONG1
  LONG2=TABLE(IJK)%LONG2
  LONGP=TABLE(IJK)%LONGP
  HT1=TABLE(IJK)%HT1
  HT2=TABLE(IJK)%HT2
  STATION=TABLE(IJK)%STATION
  COUNTRY=TABLE(IJK)%COUNTRY
  REGION=TABLE(IJK)%REGION
  DCNN=TABLE(IJK)%DCNN
  RAIN=TABLE(IJK)%RAIN
  OYEAR=TABLE(IJK)%OYEAR
  OMONTH=TABLE(IJK)%OMONTH
  ODAY=TABLE(IJK)%ODAY
  OHOUR=TABLE(IJK)%OHOUR
  CYEAR=TABLE(IJK)%CYEAR
  CMONTH=TABLE(IJK)%CMONTH
  CDAY=TABLE(IJK)%CDAY
  CHOUR=TABLE(IJK)%CHOUR
  FLAGS=TABLE(IJK)%FLAGS
  CLASS=TABLE(IJK)%CLASS
  HAW00=TABLE(IJK)%HAW00
  HAW12=TABLE(IJK)%HAW12
  MAP=TABLE(IJK)%MAP
  RUN1=TABLE(IJK)%RUN1
  RUN2=TABLE(IJK)%RUN2
  OREC=TABLE(IJK)%OREC
  NREC=TABLE(IJK)%NREC
  UA=TABLE(IJK)%UA
  SYN=TABLE(IJK)%SYN
  NCM=TABLE(IJK)%NCM
  NUM=TABLE(IJK)%NUM
  DAY=TABLE(IJK)%DAY
  SYNTIM=TABLE(IJK)%SYNTIM
  NCMTIM1=TABLE(IJK)%NCMTIM1
  NCMTIM2=TABLE(IJK)%NCMTIM2
  SQUARE=TABLE(IJK)%SQUARE
  CHASER=TABLE(IJK)%CHASER
  RECNO=TABLE(IJK)%RECNO
END IF IF_GETDTL

! RETURN REQUIRED STATION DETAILS, SETTING RETURN CODE TO "NO MORE DATA"
! IF NEXT RECORD NUMBER IN LIST IS ZERO.

IF (ISTAT == 4) THEN
  IJK=IJK+1
  IF (INDREC(IJK) == 0) THEN
    ISTAT=0
  END IF
END IF

  999 RETURN
CONTAINS
  SUBROUTINE ORDER(MIXED,COUNT,POINTERS)
!
! sets up array POINTERS to index array MIXED in record number order.
! e.g. if MIXED = (100,3,23,350) then
!      POINTERS = (2,3,1,4)
!
  IMPLICIT NONE
  INTEGER,INTENT(IN)    :: COUNT              !Dimension of arrays
  INTEGER,INTENT(IN)    :: MIXED(COUNT)       !record numbers in request
                                              ! order
  INTEGER,INTENT(INOUT) :: POINTERS(COUNT)    !index to record numbers
                                              ! in numerical order
  LOGICAL,ALLOCATABLE   :: MASK(:)            ! says which elements
                                              ! of array to use in
                                              ! MINLOC function
  INTEGER,ALLOCATABLE   :: IPOS(:)
  INTEGER               :: I,K         ! loop counters

  ALLOCATE (MASK(COUNT))
  ALLOCATE (IPOS(COUNT))
  MASK(:)=.TRUE.
!
! If there's only one record, just return 1
!
  IF (COUNT == 1) THEN
    POINTERS(1)=1
  ELSE
!
! Loop over the number of array elements, finding the location
! of the minimum number each time, then excluding that from the
! next search.
!
! MIXED should always have unique entries but put a check in just in
! case.
!
! N.B. Nag's implementation of MINLOC does not work as advertised.
! If there are two or more equal elements it should return an array of
! locations; in fact it always returns one, but still needs an array
! for the output (IPOS). So the check for IPOS(2)>0 is redundant for
! Nag's implementation, but left here in case they correct it!
!
    DO I=1,COUNT
      IPOS=MINLOC(MIXED(1:COUNT),MASK(1:COUNT))
      IF (IPOS(2) > 0) THEN
        PRINT*,'STNFIN: WARNING - DUPLICATE STN ID - UNABLE TO OPTIMISE'
        DO K=1,COUNT
          POINTERS(K)=K
        END DO
        EXIT
      ELSE
        MASK(IPOS(1))=.FALSE.
        POINTERS(I)=IPOS(1)
      END IF
    END DO
  END IF
  DEALLOCATE (MASK)
  DEALLOCATE (IPOS)

  RETURN
  END SUBROUTINE ORDER
END SUBROUTINE STNRET
