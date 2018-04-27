      SUBROUTINE ARGCHOP (NOBS1, NSEQ, NTH, IDESCR, NDESCR,
     &                    NAMES, VALIN, VALOUT, MSGBULL, LENBUL,
     &                    ICAT,ISUB)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : ARGCHOP
!
! PURPOSE     : To create a new ARGO bulletin containing one report
!
! DESCRIPTION : Given an array of decoded values (VALIN) from an
!               ARGO bulletin, ARGCHOP extracts the Nth observation
!               and copies it to a similar array (VALOUT), then converts
!               this into a new BUFR message MSGBULL of length LENBUL.
!
!               VALOUT is only for internal use but is put in the
!               argument list so that the user can have control over
!               its size. It should be the same size as VALIN but must
!               be a separate array.
!
! USAGE       : CALL ARGCHOP (NOBS1, NSEQ, NTH, IDESCR, NDESCR,
!                         NAMES, VALIN, VALOUT, MSGBULL, LENBUL,
!                         ICAT, ISUB)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  NOBS1   I   I4        Number of obs in input array (VALIN).
!      2  NSEQ    I   I4        BUFR sequence descriptor for output.
!      3  NTH     I   I4        Required OB from input array (Nth OB).
!      4  IDESCR  I   I4    *   Descriptor array
!      5  NDESCR  I   I4        Number of descriptors (for 1st ob)
!      6  NAMES   I   C(*)      Any character values output from debufr
!      7  VALIN   I   R4    *   Input array of decoded element values
!                                 (effective dimensions (NOBS1,NELEMS).)
!      8  VALOUT  I   R4    *   Work array for values to be encoded
!                                 (effective dimensions (1,NELEMS).)
!      9  MSGBULL O  C(*)       Output ARGO bulletin ('BUFR..7777').
!     10  LENBUL  O   I4        Length of output bulletin in bytes.
!     11  ICAT    I   I4        BUFR data category
!     12  ISUB    I   I4        BUFR data sub-category
!
! CALLED BY   : STOREUM
!
! CALLS       : DATIM, ENBUFV2, DESFXY
!
! HISTORY     : This version by Sheila Needham based on MSGCHOP.
!
! REVISION INFO:
!
! $Workfile: argchop.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 21/10/2008 09:49:47$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         21/10/2008 09:49:47    Sheila Needham
!       Updated following peer review CR6853
!  1    Met_DB_Project 1.0         09/10/2008 13:52:54    Sheila Needham  New
!       for ARGO storage
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters
      INTEGER MAXDES
      PARAMETER (MAXDES=7000)  ! must be big enough for one fully
!                                 expanded descriptor sequence
!                                                             Variables

      INTEGER IDES          ! 'FXXYYY'-to-integer conversion function
      INTEGER IDESCR(*)     ! Descriptor array
      INTEGER IC            ! Position in ICPTR array
      INTEGER ICAT          ! BUFR category
      INTEGER ICPTR(10)     ! positions of character values
      INTEGER ISUB          ! Data sub-category
      INTEGER IX            ! Position in IDESCR array
      INTEGER J             ! Loop variable for general use
      INTEGER JDES          ! Loop variable for loop over descriptorss
      INTEGER JVAL          ! Loop variable for loop over ob values
      INTEGER LENBUL        ! Length of re-encoded message
      INTEGER LISTDES(MAXDES) ! BUFR descriptor sequence for message
      INTEGER NDES          ! Number of descriptors in LISTDES array
      INTEGER NDESCR        ! Number of descriptors for 1st ob in IDESCR
      INTEGER NTIME(5)      ! Time of receipt (yr, mon, day, hr, min)
      INTEGER NTH           ! Observation number required from VALIN
      INTEGER NF,NX,NY      ! Components of descriptor FXY
      INTEGER NELEMS        ! Number of values per ob in VALIN array
      INTEGER NOBS1         ! Number of obs in input values array VALIN
      INTEGER NOW(8)        ! Current date and time (from call to DATIM)
      INTEGER NSEQ          ! Number of obs in output BUFR message
      INTEGER NVAL1         ! Pointer to location in VALIN array
      INTEGER NVAL2         ! Pointer to location in VALOUT array

      REAL VALIN(*)         ! Decoded values from ARGO bulletin
      REAL VALOUT(*)        ! Values for encoding new ARGO bulletin

      LOGICAL FIRST         ! .TRUE. if first call to subroutine

      CHARACTER*1 CDUMMY    ! Dummy character argument for ENBUFV2
      CHARACTER*80 HEAD     ! Revision information                   !2
      CHARACTER*(*) MSGBULL ! Re-encoded ARGO bulletin
      CHARACTER*(*) NAMES   ! Input character values
!                                                       Saved variables
      SAVE FIRST, LISTDES
!                                                       Data statements
      DATA FIRST /.TRUE./

!-----------------------------------------------------------------------
!     REVISION INFORMATION (FIRST CALL ONLY) AND INITIALISATIONS
!-----------------------------------------------------------------------
!                                                  Revision information
      IF (FIRST) THEN
        HEAD='$Workfile: argchop.f$ ' //
     &       '$Revision: 2$ $Date: 21/10/2008 09:49:47$'
        FIRST = .FALSE.
      END IF
!                                                       Initialisations
      IC = 0    ! count of character elements
!
! Find position in IDESCR for the start of this OB
!
      IX = 1    ! pointer to start of descriptor list in IDESCR
      NDES=NDESCR
      DO J=2,NTH
        IX = IX+NDES
        NDES = IDESCR(IX)
        IX=IX+1
      END DO
!
!  Check there will be room for the encoder to expand the replications
!
      IF (NDES.GT.MAXDES) THEN
        WRITE(6,'(''ARGCHOP: Too many replications'',I6)')NDES
        LENBUL = 0
        RETURN
      END IF
!
!  Find number of data elements corresponding to these descriptors
!
      NELEMS=0
      DO JDES=IX,IX+NDES-1
        CALL DESFXY (IDESCR(JDES), NF,NX,NY)
        IF (NF.EQ.0) THEN
           NELEMS = NELEMS + 1
        END IF
!                          Store the position of character elements

        IF (IDESCR(JDES).GT.131072)THEN
          IC=IC+1
          ICPTR(IC)=NELEMS
        ENDIF
      END DO ! JDES

!-----------------------------------------------------------------------
!     EXTRACT DATA FOR THE NTH OBSERVATION
!-----------------------------------------------------------------------

      NVAL1 = NTH       ! First data value in VALIN
      NVAL2 = 1         ! First data value in VALOUT
!                              Transfer data values to VALOUT
      DO JVAL=1,NELEMS
        VALOUT(NVAL2) = VALIN(NVAL1)
        NVAL1 = NVAL1 + NOBS1
        NVAL2 = NVAL2 + 1
      END DO ! JVAL

! Adjust pointers to character elements
! (DEBUFR puts a length and pointer to the character string in the
!   values array, but ENBUFR only needs the pointer)

      DO J=1,IC
        JVAL=ICPTR(J)
        VALOUT(JVAL)=AMOD(VALOUT(JVAL),65536.0)
      END DO
!-----------------------------------------------------------------------
!     CREATE NEW ARGO BULLETIN CONTAINING ONE OBSERVATION
!-----------------------------------------------------------------------
!                                                   Get time of receipt
      CALL DATIM (NOW)
      DO J=1,5              ! Yr, mon, day, hr, min
        NTIME(J) = NOW(9-J)
      END DO ! J
!                                                   Encode new bulletin
      LISTDES(1) = IDES(NSEQ)  ! BUFR sequence
      DO J=2,MAXDES
        LISTDES(J) = 0         ! Rest of descriptor array
      END DO ! J
      NDES = 1

      CALL ENBUFV2 (LISTDES, VALOUT, NDES, NELEMS, 1, NAMES,
     &              NTIME, MSGBULL,.FALSE., LENBUL, 3, 0, 74, ICAT,
     &             ISUB,-99,-99, .FALSE., CDUMMY, .FALSE., CDUMMY, 1)

!                                             Return to calling program
      RETURN
      END
