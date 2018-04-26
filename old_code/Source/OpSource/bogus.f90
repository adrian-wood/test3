PROGRAM BOGUS


!-----------------------------------------------------------------------
!
! PROGRAM       : BOGUS
!
! To store BOGUS data.
!
! DESCRIPTION   : 
! 
! BOGUS data is stored by a separate job (QTXXROIB).
! The input consists of headers (BOGUS or not) and
! records.  Store a BUFR message for each BOGUS record.
!
! TESTING       : The input data set is deleted at the end of the job,
!                 but the first step of the job prints it, so the data
!                 can be reused for tests by extracting to a data set
!                 (line command E) in EJES.
!
! DATA TYPE(S)  : BOGUS
!
! CALLED BY     : nothing, it's a main program
!
! CALLS         : BOGEXP to decode
!                 BOGIND to encode, index & store
!
! REVISION INFO :
!
! $Workfile: bogus.f90$   $Folder: OpSource$
! $Revision: 2$   $Date: 19/01/2012 12:07:23$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         19/01/2012 12:07:23    Stan Kellett
!       Corrected revision lines
!  1    Met_DB_Project 1.0         19/01/2012 12:06:14    Stan Kellett
!       Initial ported version of bogus.f
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------     
use bogexp_mod
use bogind_mod

IMPLICIT NONE   

!declare character
CHARACTER(LEN=80)              :: HEADER          !Header of bogus data
CHARACTER(LEN=3)               :: bogusType       !Type of bogus report
CHARACTER(LEN=4)               :: IDENT_PRESSURE  !Pressure value for IDENT

!declare integer
INTEGER                        :: NOW(8)          !System time
INTEGER                        :: TOR(5)          !T.O.R. or report
INTEGER                        :: YMD(3)          !Year, Month, Day
INTEGER                        :: I               !Used in loops  
INTEGER                        :: IFT             !Input dataset unit no.
INTEGER                        :: OFT             !Output dataset unit number
INTEGER                        :: IOS             !Iostat check from read hed
INTEGER                        :: REP_COUNT       !Count of reports
INTEGER                        :: CENTURY                                    

!declare real
REAL                           :: VALUES(13)      !Decoded values array  

!declare logical
LOGICAL                        :: INPUT_BOGUS     !Latest header is bogus

!save values
SAVE

!initialize variables
IOS=0
REP_COUNT=0
INPUT_BOGUS=.FALSE.
IFT=9                                 !Unit no. of INPUT set.
OFT=10                                !Unit no. of output set.

! Open both datsets, sequential input IFT & direct access storage OFT.

OPEN (IFT,FORM='FORMATTED')
OPEN (OFT,ACCESS='DIRECT',RECL=27998)  !Open storage dataset

! Get time of receipt from system time (same for all obs in this input)

CALL DATIM(NOW)
DO I = 0, 4
  TOR(I+1) = NOW(8-I)
ENDDO

! Read in a record and check to see if it is a header. A header has 'Z'
! (after an hour) in the 3rd byte, a bogus header '10' in bytes 36-37.
! If this record is not a header & the last header was for BOGUS data,
! then it's a BOGUS record: decode, BUFR encode & store the data.
! Skip any non-BOGUS header & the following records.

DO WHILE (IOS == 0)
  READ (IFT,'(A80)',IOSTAT=IOS) HEADER
  IF (IOS == 0) THEN
    IF (HEADER(3:3) == 'Z') THEN
      IF (HEADER(36:37) == '10') THEN
        INPUT_BOGUS = .TRUE.
        READ(HEADER(7:8),'(I2)')YMD(3)     ! day
        READ(HEADER(10:11),'(I2)')YMD(2)   ! month
        READ(HEADER(13:14),'(I2)')YMD(1)   ! year
        IF (YMD(1) >= 0 .AND. YMD(1) < 1900) THEN
          YMD(1) = YMD(1)+CENTURY(YMD(1))
        ENDIF
      ELSE
        INPUT_BOGUS = .FALSE.
      ENDIF
    ELSE IF (INPUT_BOGUS) THEN   ! if this record is not a header
      bogusType = HEADER(4:6)
      CALL BOGEXP(HEADER, VALUES, bogusType, IDENT_PRESSURE)
      REP_COUNT = REP_COUNT + 1
      CALL BOGIND(HEADER, VALUES, bogusType, OFT, YMD, TOR, IDENT_PRESSURE)
    ENDIF
  ENDIF
ENDDO

! Storage is now complete for this input: close the datasets &
! write out the number of reports found (including duplicates)

CLOSE(IFT)                    !Close input dataset
CLOSE(OFT)                    !Close output dataset
WRITE (6,*) 'BOGUS storage run:',REP_COUNT,'BOGUS obs this time'

STOP
END PROGRAM BOGUS
