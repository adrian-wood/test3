SUBROUTINE STORBUFR (DATYPE, BUL18, BULL, ITEMS, STORFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE     : STORBUFR
!
! PURPOSE     : To do additional data type dependent checks for
!               storage of BUFR bulletins.
!
! DESCRIPTION : STORBUFR is used to check the storage requirement and
!               index entry content for certain types of BUFR data
!               where additional information or processing beyond that
!               done in BUFRDAT is required.
!
!               In fact, STORBUFR does the same job for BUFR bulletins
!               that STORCHEK does for non-BUFR bulletins.
!
!               STORBUFR is only called if the second flag in the
!               headers data set is set to TRUE.
!
! USAGE       : CALL STORBUFR (DATYPE, BUL18, BULL, ITEMS, STORFLAG)
!
! ARGUMENTS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!               NAME       I/O  TYPE        DESCRIPTION
!               -----      ---  ----        -----------
!               DATYPE      I   C*8    MetDB data type (e.g. 'MERIS')
!               BUL18       I   C*18   Bulletin header (TTAAii...YYGGgg)
!               BULL        I   C*(*)  BUFR bulletin ('BUFR......7777')
!               ITEMS(0:*)  I   I*4    Array of data processing items
!               STORFLAG   I/O  L*4    BUFR message storage flag
!
! CALLED BY   : BUFRDAT
!
! REVISION INFO :
!
! $Workfile: storbufr.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 12/04/2013 09:30:45$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         12/04/2013 09:30:45    Brian Barwell
!       MSGRAD section also used for MSGCTP. New UKWINDS section added. MERIS
!       section deleted.
!  4    MetDB_Refresh 1.3         26/06/2012 17:02:14    Brian Barwell   Set
!       correct identifier code for land/ship/mobile BUFR TEMPs.
!  3    MetDB_Refresh 1.2         31/01/2011 17:20:25    John Norton     After
!       rework on BUFRDAT batch 4 done.
!  2    MetDB_Refresh 1.1         27/01/2011 17:55:16    John Norton
!       BUFRDAT batch 4 ported now ready for review.
!  1    MetDB_Refresh 1.0         26/01/2011 13:49:39    John Norton
!       Pre-porting version of f77 code batch BUFRDAT4
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2013 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE ichar3_mod !Function

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=8),  INTENT(IN)    :: DATYPE     !a01 MetDB data type
CHARACTER(LEN=18), INTENT(IN)    :: BUL18      !a02 Bulletin header ('TTAAii CCCC YYGGgg')
CHARACTER(LEN=*),  INTENT(IN)    :: BULL       !a03 BUFR bulletin ('BUFR......7777')
INTEGER,           INTENT(INOUT) :: ITEMS(0:*) !a04 Data processing items (from headers d/s)
LOGICAL,           INTENT(INOUT) :: STORFLAG   !a05 .TRUE. if BUFR message is to be stored

! Local declarations:
!                                                             Variables
INTEGER          ::  I      ! Location in BUFR message
INTEGER          ::  II     ! 'ii' from GTS header ('TTAAii')
INTEGER          ::  LENGTH ! Length of message in bytes

!=======================================================================
!            SPECIAL PRE-PROCESSING FOR BUFR SONDE DATA
!=======================================================================

IFLABEL1: &
IF (DATYPE == 'SONDE') THEN                                          !4

!-----------------------------------------------------------------------
!   To index TEMP and PILOT data appropriately it is necessary to    !4
! distinguish between data from land stations which report WMO       !4
! station numbers (001001 & 001002) and ships & mobile stations      !4
! which report station identifier (001011). The indexing codes used  !4
! by subroutine INDEX2 for these cases are 1 and 15 respectively.    !4
!   In the GTS bulletin headers 'TTAAii', the 'ii' value is used to  !4
! check for land stations ('ii'= 01-19), ships ('ii'= 20-39) and     !4
! mobile stations ('ii'= 40-59).                                     !4
!-----------------------------------------------------------------------

  ITEMS(9) = 1     ! WMO station numbers (default)                   !4

!  Check for 'ii' in GTS header in range 20-59 (ship or mobile station)

  IF (BUL18(5:5) >= '2' .AND. BUL18(5:5) <= '5' .AND.   &            !4
      BUL18(6:6) >= '0' .AND. BUL18(6:6) <= '9') ITEMS(9) = 15       !4

!=======================================================================
!              SPECIAL PRE-PROCESSING FOR JMAWINDS DATA
!=======================================================================

ELSE IF (DATYPE == 'JMAWINDS') THEN                                  !4

!-----------------------------------------------------------------------
!   About 30% of JMAWINDS bulletins were originally being rejected by
! the duplicate data check as they had the same date, time, satellite
! number, lat/long box, number of observations and data selection
! parameter as other bulletins even though the data content was
! different. It appears that the 2-digit number at the end of the
! bulletin header (the 'ii' of 'TTAAii') is a reliable indicator for
! checking these apparent duplicates so this is incorporated here in
! the data selection parameter to make a composite number:
!
!             Data selection parameter = k + 60*n
!
! where 'n' is the wind derivation type (1=Infrared, 2=Visible, 3=Water
! vapour) and 'k' is a positive integer less than 60 dependent only
! on 'ii'. This value is put in ITEMS(3) for insertion into the
! index entry later. Duplicate data will then be detected only for
! bulletins with the same 'n' and 'ii'.
!   When using the 'SELECT' keyword in retrievals, the data selection
! parameter will be divided by 60 to recover 'n' before being checked.
!-----------------------------------------------------------------------
!                                        Check for digits in GTS header

IFLABEL2: &
  IF (BUL18(5:5) >= '0' .AND. BUL18(5:5) <= '9' .AND.   &
      BUL18(6:6) >= '0' .AND. BUL18(6:6) <= '9') THEN
!                                                           Decode 'ii'
    READ (BUL18(5:6),'(I2)') II

    IF (II > 70) THEN         ! IUC(N/S)71-95: H2O winds
      ITEMS(3) = (II-70) + 180
    ELSE IF (II > 50) THEN    ! IUC(N/S)51-65: Infrared winds
      ITEMS(3) = (II-50) + 60
    ELSE IF (II > 40) THEN    ! IUC(N/S)41-46: Visible winds
      ITEMS(3) = (II-40) + 120
    ELSE                      ! IUC(N/S)01-34: Old headers -
      ITEMS(3) = 0            !                ignore
    END IF
  ELSE
!                                 'ii' not digits: set ITEMS(3) to zero
    ITEMS(3) = 0
  END IF IFLABEL2

!=======================================================================
! SPECIAL PRE-PROCESSING FOR MODIS DIRECT BROADCAST WINDS (GOESBUFR)
!=======================================================================

ELSE IF (DATYPE == 'GOESBUFR') THEN

!-----------------------------------------------------------------------
!   MODIS direct broadcast winds are produced in GOESBUFR format by
! a few centres indicated by code numbers (10, 11, etc.) in the
! 'originating sub-centre' in section 1 of the BUFR message. To
! enable data from particular centres to be selected on retrieval,
! a value of 10*(code number-9) is added to the 'index entry' byte
! (element 3 of array ITEMS).
!-----------------------------------------------------------------------

  I = ICHAR(BULL(13:13))       ! sub-centre in section 1
  ITEMS(3) = MOD(ITEMS(3),10)  ! restore original value

  IF (I >= 10 .AND. I <= 17) ITEMS(3) = ITEMS(3) + 10*(I-9)

!=======================================================================
! SPECIAL PRE-PROCESSING FOR MSGRAD AND MSGCTP DATA INCLUDING FULL-DISK
!   DATA (MSGRADFD AND MSGCTPFD) AND UK DATA (MSGRADUK AND MSGCTPUK)
!=======================================================================

ELSE IF (DATYPE(1:3) == 'MSG') THEN                                  !5

!-----------------------------------------------------------------------
!   The standard duplicate data checking for MSGRAD and MSGCTP often
! fails because the time interval between messages is less than one
! second. To avoid rejection as duplicates, we here put the 28th byte
! of section 4 of the BUFR bulletin in the data selection parameter so
! that this gets checked as well.
!   If all the times in the message are the same (most of the messages)
! this byte is part of the 25-bit minimum latitude which occupies bits
! 205-229 of that section.  (Byte 57 covers bits 213-220.) This
! increments gradually from one bulletin to the next so should be
! different for consecutive scans.
!   Note: the location in the message of this byte relies on the use
! of a BUFR edition greater than 1 and no section 2 in the message.
!   This section is also used for all MSGRAD and MSGCTP products.
!-----------------------------------------------------------------------

  I = 8                          ! Skip section 0 ...
  I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 1 ...
  I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 3
  I = I + 28                     ! Byte 57 of section 4

  LENGTH = ICHAR3(BULL(5:7))
  IF (I < LENGTH) ITEMS(3) = ICHAR(BULL(I:I))

!=======================================================================
!              SPECIAL PRE-PROCESSING FOR UKWINDS DATA               !5
!=======================================================================

ELSE IF (DATYPE == 'UKWINDS') THEN                                   !5

!-----------------------------------------------------------------------
!   Duplicate data checking for UKWINDS data can cause spurious
! rejection because observations in a bulletin all have the same date,
! time and satellite number, and the bulletin usually has the same
! number of observations and sometimes the same lat/long box as other
! bulletins. To avoid rejection as duplicates, we here put the 28th
! byte of section 4 of the bulletin in the data selection parameter so
! that this gets checked as well.
!   This byte is part of the 25-bit minimum latitude which occupies
! bits 209-233 of that section. (Byte 28 covers bits 217-224.) This
! varies from one bulletin to another so should be different.
!   Note: the location in the message of this byte relies on previous
! data items in a bulletin being identical for all observations so
! that increments are coded with zero bits. It also assumes a BUFR
! edition greater than 1 and no section 2 in the message.
!-----------------------------------------------------------------------

  I = 8                          ! Skip section 0 ...                !5
  I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 1 ...             !5
  I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 3                 !5
  I = I + 28                     ! Byte 28 of section 4              !5

  LENGTH = ICHAR3(BULL(5:7))                                         !5
  IF (I < LENGTH) ITEMS(3) = ICHAR(BULL(I:I))                        !5
END IF IFLABEL1
!                                             Return to calling program
RETURN
END SUBROUTINE STORBUFR
