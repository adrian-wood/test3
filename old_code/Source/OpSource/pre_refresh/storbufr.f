      SUBROUTINE STORBUFR (DATYPE, BUL18, BULL, ITEMS, STORFLAG)    !2.2

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
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
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
! $Workfile: storbufr.f$ $Folder: pre_refresh$
! $Revision: 4$ $Date: 28/11/2008 13:47:16$
! $Author: Brian Barwell$
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         28/11/2008 13:47:16    Brian Barwell
!       Process MSGRADFD in same way as MSGRAD.
!  3    Met_DB_Project 1.2         15/12/2006 15:20:58    Brian Barwell   Add
!       section to modify data selection for MODIS direct broadcast winds.
!  2    Met_DB_Project 1.1         18/10/2006 16:36:08    Brian Barwell
!       Section added for MSGRAD data.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:39    Sheila Needham  
! $
! Revision 2.4  2005/05/04 08:53:37  usmdb
! 2.4.  16 May 2005.  Brian Barwell.  Remedy CHG012685.
! Modified MERIS action after BUFR sequence change in early April.
!
! Revision 2.3  2005/03/18 09:53:16  usmdb
! 2.3.  21 March 2005.  Brian Barwell.
! Modified processing for JMAWINDS after headers changed, 01/01/05.
!
! Revision 2.2  2004/03/03 15:05:14  usmdb
! 2.2.  15 March 2004.  Brian Barwell.  Change 17/04.
! Complete rewrite with code for pre-processing JMAWINDS data.
! Also a section for MERIS data which will be needed soon.
!
! Revision 2.1  2004/01/06  11:14:03  11:14:03  usmdb (MetDB account c/o usjh)
! 2.1.  19 January 2004.  Brian Barwell.  Change 115/03.
! Rewritten as dummy routine after need for version 2.0 lapsed.
!
! Revision 2.0  2002/10/07 15:30:39  usmdb
! Initial Version.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                             Variables
      INTEGER I             ! Location in BUFR message              !2.4
      INTEGER ICHAR3        ! Function to convert C*3 to I*4        !2.4
      INTEGER II            ! 'ii' from GTS header ('TTAAii')       !2.2
      INTEGER ITEMS(0:*)    ! Data processing items (from headers d/s)
      INTEGER LENGTH        ! Length of message in bytes            !2.4

      LOGICAL FIRST         ! .TRUE. if first call to subroutine
      LOGICAL STORFLAG      ! .TRUE. if BUFR message is to be stored

      CHARACTER*8   DATYPE  ! MetDB data type
      CHARACTER*(*) BULL    ! BUFR bulletin ('BUFR......7777')
      CHARACTER*18  BUL18   ! Bulletin header ('TTAAii CCCC YYGGgg') 2.2
      CHARACTER*80  HEAD    ! Revision information                    !2
!                                                       Saved variables
      SAVE FIRST
!                                                   Data initialisation
      DATA FIRST/.TRUE./
!                                              Set revision information
      IF (FIRST) THEN
        HEAD = '$Workfile: storbufr.f$ ' //
     &  '$Revision: 4$ $Date: 28/11/2008 13:47:16$'
        FIRST = .FALSE.
      END IF

!=======================================================================
!              SPECIAL PRE-PROCESSING FOR JMAWINDS DATA             !2.2
!=======================================================================

      IF (DATYPE.EQ.'JMAWINDS') THEN                                !2.2

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
!             Data selection parameter = k + 60*n                   !2.3
!
! where 'n' is the wind derivation type (1=Infrared, 2=Visible, 3=Water
! vapour) and 'k' is a positive integer less than 60 dependent only !2.3
! on 'ii'. This value is put in ITEMS(3) for insertion into the     !2.3
! index entry later. Duplicate data will then be detected only for  !2.3
! bulletins with the same 'n' and 'ii'.                             !2.3
!   When using the 'SELECT' keyword in retrievals, the data selection
! parameter will be divided by 60 to recover 'n' before being checked.
!-----------------------------------------------------------------------
!                                        Check for digits in GTS header

        IF (BUL18(5:5).GE.'0' .AND. BUL18(5:5).LE.'9' .AND.         !2.3
     &      BUL18(6:6).GE.'0' .AND. BUL18(6:6).LE.'9') THEN         !2.2
!                                                           Decode 'ii'
          READ (BUL18(5:6),'(I2)') II                               !2.2

          IF (II.GT.70) THEN        ! IUC(N/S)71-95: H2O winds      !2.3
            ITEMS(3) = (II-70) + 180                                !2.3
          ELSE IF (II.GT.50) THEN   ! IUC(N/S)51-65: Infrared winds !2.3
            ITEMS(3) = (II-50) + 60                                 !2.3
          ELSE IF (II.GT.40) THEN   ! IUC(N/S)41-46: Visible winds  !2.3
            ITEMS(3) = (II-40) + 120                                !2.3
          ELSE                      ! IUC(N/S)01-34: Old headers -  !2.3
            ITEMS(3) = 0            !                ignore         !2.3
          END IF
        ELSE                                                        !2.2
!                                 'ii' not digits: set ITEMS(3) to zero
          ITEMS(3) = 0                                              !2.2
        END IF                                                      !2.2

!=======================================================================
! SPECIAL PRE-PROCESSING FOR MODIS DIRECT BROADCAST WINDS (GOESBUFR) !3
!=======================================================================

      ELSE IF (DATYPE.EQ.'GOESBUFR') THEN                            !3

!-----------------------------------------------------------------------
!   MODIS direct broadcast winds are produced in GOESBUFR format by  !3
! a few centres indicated by code numbers (10, 11, etc.) in the      !3
! 'originating sub-centre' in section 1 of the BUFR message. To      !3
! enable data from particular centres to be selected on retrieval,   !3
! a value of 10*(code number-9) is added to the 'index entry' byte   !3
! (element 3 of array ITEMS).                                        !3
!-----------------------------------------------------------------------

        I = ICHAR(BULL(13:13))       ! sub-centre in section 1       !3
        ITEMS(3) = MOD(ITEMS(3),10)  ! restore original value        !3

        IF (I.GE.10 .AND. I.LE.17) ITEMS(3) = ITEMS(3) + 10*(I-9)    !3

!=======================================================================
!               SPECIAL PRE-PROCESSING FOR MERIS DATA
!=======================================================================

      ELSE IF (DATYPE.EQ.'MERIS') THEN                              !2.2

!-----------------------------------------------------------------------
!   Duplicate data checking for MERIS is complicated as some bulletins
! have the same date & time (scan time is <1 sec.), satellite number,
! lat/long box and number of observations even though the data content
! is different. To avoid rejection as duplicates, we here put the 57th
! byte of section 4 of the bulletin in the data selection parameter so
! that this gets checked as well.
!   This byte is part of the 25-bit minimum latitude which occupies
! bits 439-463 of that section.  (Byte 57 covers bits 449-456.) This
! increments gradually from one scan to the next so should be different
! for consecutive scans.                                            !2.4
!   Note: the location in the message of this byte relies on previous
! data items (satellite, instrument and software details, orbit number,
! date and time) being identical for all points in the scan so that
! increments are coded with zero bits. It also assumes a BUFR edition
! greater than 1 and no section 2 in the message.                   !2.4
!-----------------------------------------------------------------------

        I = 8                          ! Skip section 0 ...         !2.4
        I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 1 ...      !2.4
        I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 3          !2.4
        I = I + 57                     ! Byte 57 of section 4       !2.4

        LENGTH = ICHAR3(BULL(5:7))                                  !2.4
        IF (I.LT.LENGTH) ITEMS(3) = ICHAR(BULL(I:I))                !2.4

!=======================================================================
!        SPECIAL PRE-PROCESSING FOR MSGRAD AND MSGRADFD DATA          !4
!=======================================================================

      ELSE IF (DATYPE(1:6).EQ.'MSGRAD') THEN                          !4

!-----------------------------------------------------------------------
!   The standard duplicate data checking for MSGRAD often fails       !2
! because the time interval between messages is only about 0.25 sec.  !2
! To avoid rejection as duplicates, we here put the 28th byte of      !2
! section 4 of the bulletin in the data selection parameter so that   !2
! this gets checked as well.                                          !2
!   If all the times in the message are the same (about 70% of        !2
! messages) this byte is part of the 25-bit minimum latitude which    !2
! occupies bits 205-229 of that section.  (Byte 57 covers bits        !2
! 213-220.) This increments gradually from one scan to the next so    !2
! should be different for consecutive scans.                          !2
!   This section is also used for MSGRADFD data.                      !4
!   Note: the location in the message of this byte relies on the use  !2
! of a BUFR edition greater than 1 and no section 2 in the message.   !2
!-----------------------------------------------------------------------

        I = 8                          ! Skip section 0 ...           !2
        I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 1 ...        !2
        I = I + ICHAR3(BULL(I+1:I+3))  ! ... and section 3            !2
        I = I + 28                     ! Byte 57 of section 4         !2

        LENGTH = ICHAR3(BULL(5:7))                                    !2
        IF (I.LT.LENGTH) ITEMS(3) = ICHAR(BULL(I:I))                  !2
      END IF                                                        !2.2
!                                             Return to calling program
      RETURN
      END
