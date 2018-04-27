SUBROUTINE ENBUFV4(Descr,Values,Nd,Nelem,Nobs,Names,Datime,         &
                         Mesage,Cmpres,L,Edition,MasterTable,       &
                         VerMasTab,                                 &
                         OrigCentre,OrigSubCentre,                  &
                         DataType,LocalDataSubType,IntDataSubType,  &
                         VerLocTab,ExtraSect1,CharSect1,ExtraSect2, &
                         CharSect2,Sect3Type)

!-----------------------------------------------------------------------
!
! ROUTINE        : ENBUFV4
!
! PURPOSE        : This subroutine makes a complete BUFR message, given
!                  a descriptor sequence (which will be expanded if
!                  necessary) and an array of values corresponding to
!                  the expanded sequence (i.e. the user must know the
!                  expansion beforehand)
!                  Fields in earlier sections of the message are set as
!                  requested - or to suitable defaults.  The defaults
!                  for edition & master table version should be kept
!                  up to date (table version updated yearly, edition
!                  less frequently: see Manual on Codes for latest...)
!
! CALLED BY      : anything
!
! CALLS          : ENBUFR
!
! ARGUMENTS      :
!
!  1) Descr(*)     integer   (ip/op) - Descriptor sequence (may need
!                                      expanding
!  2) Values(Nobs,Nelem)  real (ip)  - Values to BUFR encode
!  3) Nd           integer   (ip/op) - Number of descriptors (returned
!                                      as 0 if there is an error)
!  4) Nelem        integer   (ip)    - Number of elements in array
!  5) Nobs         integer   (ip)    - Number of observations in array
!  6) Names        char*(*)  (ip)    - character values to encode
!                                      (with pointers in array)
!  7) Datime(6)    integer   (ip)    - year, mon, day, hr, min, sec
!  8) Mesage       char*(*)  (op)    - string for BUFR message
!  9) Cmpres       logical   (ip)    - TRUE if compression wanted
! 10) L            integer   (op)    - length of BUFR message
! 11) OrigCentre   integer   (ip)    - Originating Centre (sect. 1)
!                                      code -99 for default (=74)
! 12) OrigSubCentre integer  (ip)    - Originating Sub Centre (sect.1)
!                                      code -99 for default (=0)
! 13) DataType     integer   (ip)    - Data category type (sect. 1)
!                                      code -99 for default (=255)
! 14) LocalDataSubType int   (ip)    - Data category subtype (sect. 1)
!                                      code -99 for default (=0)
! 15) IntDataSubType   int   (ip)    - Data category subtype (sect. 1)
!                                      code -99 for default (=0)
! 16) Edition      integer   (ip)    - Edition No. (sect. 1)
!                                      code -99 for default (now 4)
! 17) MasterTable  integer   (ip)    - BUFR Master table (sect. 1)
!                                      code -99 for default (=0)
! 18) VerMasTab    integer   (ip)    - Version no. of master tables (s.1)
!                                      code -99 for default (picked up
!                                      from file on TABLEB)
! 19) VerLocTab    integer   (ip)    - Version no. of local tables (s.1)
!                                      code -99 for default (=0)
! 20) ExtraSect1   logical   (ip)    - code TRUE if there is extra data
!                                      to be added to the end of
!                                      section 1. If so, the data in
!                                      CharSect1 will be added.
! 21) CharSect1    char*(*)  (ip)    - Extra data to add to end of
!                                      section 1.
! 22) ExtraSect2   logical   (ip)    - code TRUE if there is data to
!                                      put in section 2. If so, the
!                                      data in CharSect2 will be added.
! 23) CharSect2    char*(*)  (ip)    - data to put in section 2.
! 24) Sect3Type    integer   (ip)    - section 3 byte 7 (type of data)
!                                      code 1 for observed, 0 for other
!                                      code -99 for default (=1)
!
! REVISION INFO :
!
! $Workfile: enbufv4.F90$ $Folder: OpSource$
! $Revision: 4$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  3    MetDB_Refresh 1.2         20/10/2010 11:35:39    Sheila Needham
!       Removed print statements
!  2    MetDB_Refresh 1.1         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  1    MetDB_Refresh 1.0         18/10/2010 09:34:28    Sheila Needham  New
!       for BUFR ed 4
! $
!
! ----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE enbufr_mod
USE tabbver_mod
#if defined (EBCDIC)
USE eb2asc_mod
#endif

IMPLICIT NONE

INTEGER,PARAMETER ::  MaxLen=27998    !- max length of BUFR message.

INTEGER ::  Byte7              !- byte 7 of section 3
INTEGER ::  CurrentEdtn        !- edition number from ENBUFR
INTEGER ::  LocalDataSubType   !- BUFR subtype (local use)
INTEGER ::  I                  !- Loop counter
INTEGER ::  IntDataSubType     !- BUFR subtype (international use)
INTEGER ::  DataType           !- BUFR type
INTEGER ::  Datime(6)          !- date/time array
INTEGER ::  Descr(*)           !- array of descriptors
INTEGER ::  Edition            !- BUFR edition
INTEGER ::  Flopt              !- 1 if there is a section 2
INTEGER ::  IBEFOR             !- bits before value in string
INTEGER ::  L                  !- length of BUFR message
INTEGER ::  Lextra             !- Length of extra data in input message
INTEGER ::  Lin                !- length of sect 1 or 2 in Mesage
INTEGER ::  L1,L2,L3,L4        !- lengths of sections in MessOut
INTEGER ::  MasterTable        !- BUFR master table number
INTEGER ::  Nd                 !- number of descriptors
INTEGER ::  Nelem              !- number of elements
INTEGER ::  Nobs               !- number of observations
INTEGER ::  OptPtr             !- pointer to optional section byte
INTEGER ::  OrigCentre         !- originating centre
INTEGER ::  OrigSubCentre      !- originating sub-centre
INTEGER ::  Offset             !- Offset into Section 1
INTEGER ::  Ptr                !- pointer to Mesage
INTEGER ::  Ptr2               !- pointer to MessOut
INTEGER ::  Sect3Type          !- type of data (section 3)
INTEGER ::  VALUE              !- function
INTEGER ::  VerMasTab          !- version no. of master tables
INTEGER ::  VerLocTab          !- version no. of local tables
INTEGER ::  tableBVer          !- actual version no. of table B

LOGICAL ::  Cmpres             !- TRUE for compression
LOGICAL ::  ExtraSect1         !- TRUE if extra sect 1 data
LOGICAL ::  ExtraSect2         !- TRUE if sect 2 to be added
LOGICAL ::  First              !- TRUE if first call

REAL ::    Values(Nobs,Nelem)  !- values to encode

CHARACTER(LEN=*) ::  CharSect1  !- section 1 extra data
CHARACTER(LEN=*) ::  CharSect2  !- section 2 data
CHARACTER(LEN=MaxLen) :: MessOut !- BUFR message
CHARACTER(LEN=*) :: Names      !- strings to encode
CHARACTER(LEN=4) :: BUFR       !- character string 'BUFR'
CHARACTER(LEN=*) :: Mesage     !- input string for BUFR message
CHARACTER(LEN=4) :: SEVENS     !- character string '7777'

! SAVE statement to ensure all variables are still set on next call

SAVE

DATA First/.TRUE./           !- TRUE for first call to ENBUFV4

COMMON /EBV2/MessOut

! First time only, Initialise BUFR & SEVENS.

IF (First) THEN
  First=.FALSE.
  BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)
  SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)
  IF (VerMasTab == -99 ) THEN
    CALL TABBVER(tableBVer)
  ELSE
    tableBVer = VerMasTab
  ENDIF
END IF

! Call ENBUFR to code BUFR message. Initialises some section 1 items
! for Edition 0 encoding. e.g. date/time.

CALL ENBUFR(Descr,Values,Nd,Nelem,Nobs,Names,Datime(1:5),Mesage,   &
     &            Cmpres,L,tableBVer)

! Check that ENBUFR has made a BUFR message

Ptr=INDEX(Mesage,BUFR)

IF (ND == 0 .OR. Ptr == 0 .OR. INDEX(Mesage,SEVENS) == 0) THEN
  PRINT *,'In ENBUFV4: message not made by ENBUFR'
  ND=0
  RETURN
END IF

Ptr2=1                       !- pointer in output message

!=======================================================================
! BUFR section 0
!=======================================================================

MessOut(1:4)=BUFR

! If the user wants edition 2 or more,
! leave room for the total length at the start
! & set the edition in the last byte of those four.

IF (Edition >= 2) THEN
  MessOut(8:8)=CHAR(Edition)
  Ptr2=Ptr2+8
ELSE IF (Edition == -99) THEN
  MessOut(8:8)=CHAR(4)
  Ptr2=Ptr2+8
ELSE
  Ptr2=Ptr2+4
END IF

! Move to BUFR section 1 of the encoded message

CurrentEdtn=ICHAR(Mesage(Ptr+7:Ptr+7))
IF (CurrentEdtn < 0) CurrentEdtn=CurrentEdtn+256

Ptr=Ptr+4
IF (CurrentEdtn >= 2) Ptr=Ptr+4

!=======================================================================
! BUFR section 1
!=======================================================================

! Find length of section 1 & copy the first 17 octets to the output
! message. See if the encoded message has an optional section 2.
! Move update sequence and section 2 indicator for Ed.4.
! Length of section (L1) is either 18 (Ed 0-3) or longer if
! there's extra data.

IBEFOR=0
Lin=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
L1=Lin
Lextra = 0
If (L1 > 18)Lextra = L1 - 18

MessOut(Ptr2:Ptr2+17-1)=Mesage(Ptr:Ptr+17-1)

IF (Edition >= 4)THEN
  Flopt=ICHAR(Mesage(Ptr+9:Ptr+9))
  OptPtr=Ptr2+9
  MessOut(Ptr2+8:Ptr2+8) = Mesage(Ptr+6:Ptr+6)  ! Seq. no.
  MessOut(Ptr2+9:Ptr2+9) = Mesage(Ptr+7:Ptr+7)  ! Section 2 flag
  MessOut(Ptr2:Ptr2+2) = CHAR(0)//CHAR(0)//CHAR(22) ! total length
ELSE
  Flopt=ICHAR(Mesage(Ptr+7:Ptr+7))
  OptPtr=Ptr2+7
END IF


IF (Flopt < 0) Flopt=Flopt+256

! Update section 1 bytes if necessary.
! In edition 0 or 1 the byte after the length of section 1 is the
! edition number, in later editions it is the master table number,
! - so the edition number (set above for edition 2 or more) is
! always in the 8th byte of a message.

IF (Edition == 0 .OR. Edition == 1) THEN
  MessOut(Ptr2+3:Ptr2+3)=CHAR(Edition)
ELSE
  IF (MasterTable /= -99) MessOut(Ptr2+3:Ptr2+3)=CHAR(MasterTable)
END IF

! Default originating centre is 74 (UK Met Office)

IF (Edition >= 4) THEN
  IF (OrigCentre /= -99) THEN
    MessOut(Ptr2+4:Ptr2+4)=CHAR(OrigCentre/256)
    MessOut(Ptr2+5:Ptr2+5)=CHAR(MOD(OrigCentre,256))
  ELSE
    MessOut(Ptr2+4:Ptr2+4)=CHAR(0)
    MessOut(Ptr2+5:Ptr2+5)=CHAR(74)
  END IF
  IF (OrigSubCentre /= -99) THEN
    MessOut(Ptr2+6:Ptr2+6)=CHAR(OrigSubCentre/256)
    MessOut(Ptr2+7:Ptr2+7)=CHAR(MOD(OrigSubCentre,256))
  ELSE
    MessOut(Ptr2+6:Ptr2+6)=CHAR(0)
    MessOut(Ptr2+7:Ptr2+7)=CHAR(0)
  END IF
ELSE
  IF (OrigCentre  /=  -99) THEN
    MessOut(Ptr2+4:Ptr2+4)=CHAR(OrigSubCentre)
    MessOut(Ptr2+5:Ptr2+5)=CHAR(OrigCentre)
  ELSE
    MessOut(Ptr2+4:Ptr2+4)=CHAR(0)
    MessOut(Ptr2+5:Ptr2+5)=CHAR(74)
  END IF

END IF

Offset = 0
IF (Edition > 3) Offset = 2

! Default subtype cant be zero ("Surface data - land" in Table A.
! (But 255 is "for local use" (with subtype) rather than missing!)
! Default table version is latest (see Manual on Codes for number)

IF (DataType /= -99) THEN
  MessOut(Ptr2+8+Offset:Ptr2+8+Offset)=CHAR(DataType)
ELSE
    IF (ICHAR(Mesage(13:13))> 0) THEN
      MessOut(Ptr2+8+Offset:Ptr2+8+Offset) = Mesage(13:13)
    ELSE
      MessOut(Ptr2+8+Offset:Ptr2+8+Offset)=CHAR(255)
    END IF
END IF

IF (Edition  >  3) THEN
  IF (IntDataSubType /= -99) THEN
    MessOut(Ptr2+9+Offset:Ptr2+9+Offset)=CHAR(IntDataSubType)
  ELSE
    MessOut(Ptr2+9+Offset:Ptr2+9+Offset)=CHAR(0)
  END IF
  Offset = 3
ENDIF

IF (LocalDataSubType /= -99) THEN
  MessOut(Ptr2+9+Offset:Ptr2+9+Offset)=CHAR(LocalDataSubType)
ELSE
  MessOut(Ptr2+9+Offset:Ptr2+9+Offset)=CHAR(0)
ENDIF

MessOut(Ptr2+10+Offset:Ptr2+10+Offset)=CHAR(tableBVer)

IF (VerLocTab /= -99) THEN
  MessOut(Ptr2+11+Offset:Ptr2+11+Offset)=CHAR(VerLocTab)
ELSE
  MessOut(Ptr2+11+Offset:Ptr2+11+Offset)=CHAR(0)
END IF


! Dates and times have moved from Edition 4

IF (Edition >= 4) THEN
  MessOut(Ptr2+15:Ptr2+15) = CHAR(Datime(1)/256)
  MessOut(Ptr2+16:Ptr2+16) = CHAR(MOD(Datime(1),256))
  DO I=2,6
    MessOut(Ptr2+15+I:Ptr2+15+I) = CHAR(Datime(I))
  END DO
END IF

! If the user wants to add extra data to section 1, check to see if
! there is already extra dat, if so ignore users data.
! If not, calculate the length of the new section 1 and add the
! extra data to it. Check to see if there is an odd number of bytes and
! if so, pad with a zero and update the new length. Finally, put the
! length into section 1 bytes 1-3 and move to next section.

Offset = 17
If (edition  > 3) Offset = 22

IF (ExtraSect1) THEN
  IF (Lextra > 0)  THEN
    PRINT *,'ENBUFV4 found extra data already in section 1'
    PRINT *,' so input extra data for section 1 ignored'
    MessOut(Ptr2+Offset:Ptr2+Offset+Lextra-1) = Mesage(Ptr+17:Ptr+L1-1)
    L1= Offset + Lextra
    MessOut(Ptr2:Ptr2+2) = CHAR(0)//CHAR(L1/256)//CHAR(MOD(L1,256))
  ELSE
    L1=Offset+LEN(CharSect1)

#if defined (EBCDIC)

    CALL EB2ASC(LEN(CharSect1),CharSect1)
#endif
    MessOut(Ptr2+Offset:Ptr2+L1-1)=CharSect1
    IF (MOD(L1,2) /= 0) THEN
      MessOut(Ptr2+L1:Ptr2+L1)=CHAR(0)
      L1=L1+1
    END IF
    MessOut(Ptr2:Ptr2+2)=CHAR(0)//CHAR(L1/256)//CHAR(MOD(L1,256))
  END IF
ELSE
  IF (Edition >= 4) THEN
    L1=22
    MessOut(Ptr2:Ptr2+2) = CHAR(0)//CHAR(0)//CHAR(L1)
  ENDIF
END IF

Ptr=Ptr+Lin        !- move to next section of Mesage
Ptr2=Ptr2+L1       !- move to next section of MessOut

!=======================================================================
! BUFR section 2
!=======================================================================

! If there is already an optional section 2, then find its length and
! copy it from the encoded message to section 2 of the output message.

L2=0
Lin=0
IF (Flopt >= 128) THEN
  IBEFOR=0
  Lin=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
  L2=Lin

  MessOut(Ptr2:Ptr2+Lin-1)=Mesage(Ptr:Ptr+Lin-1)

  IF (ExtraSect2) THEN
    PRINT *,'ENBUFV4 found extra data already in section 2'
    PRINT *,' so input section 2 ignored'
  END IF

! If the user wants to add extra data to section 2 and there is not
! already a section 2, calculate the length of the new section 2, and
! add the extra data to it. Check to see if there is an odd number of
! bytes and if so, pad with a zero and update the new length. Finally,
! put the length into section 1 bytes 1-3, set bit 1 of byte 8 (or 10)
! in BUFR section 1 and move to next section.

ELSE IF (ExtraSect2) THEN
  MessOut(Ptr2+3:Ptr2+3)=CHAR(0)
  L2=4+LEN(CharSect2)

#if defined (EBCDIC)

  CALL EB2ASC(LEN(CharSect2),CharSect2)
#endif
  MessOut(Ptr2+4:Ptr2+L2-1)=CharSect2
  IF (MOD(L2,2) /= 0) THEN
    MessOut(Ptr2+L2:Ptr2+L2)=CHAR(0)
    L2=L2+1
  END IF
  MessOut(Ptr2:Ptr2+2)=CHAR(0)//CHAR(L2/256)//CHAR(MOD(L2,256))
  MessOut(OptPtr:OptPtr)=CHAR(128)
END IF

Ptr=Ptr+Lin        !- move to next section of Mesage
Ptr2=Ptr2+L2       !- move to next section of MessOut

!=======================================================================
! BUFR section 3
!=======================================================================

! Find the length of section 3 & copy it from the encoded message
! to the output message.

IBEFOR=0
L3=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
MessOut(Ptr2:Ptr2+L3-1)=Mesage(Ptr:Ptr+L3-1)

! Update byte 7 of section 3 if the user wants to.

IF (Sect3Type /= -99) THEN
  Byte7=ICHAR(Mesage(Ptr+6:Ptr+6))
  IF (Byte7 < 0) Byte7=Byte7+256

  IF (Byte7 >= 128 .AND. Sect3Type == 0) THEN
    MessOut(Ptr2+6:Ptr2+6)=CHAR(Byte7-128)
  ELSE IF (Byte7 < 128 .AND. Sect3Type == 1) THEN
    MessOut(Ptr2+6:Ptr2+6)=CHAR(Byte7+128)
  END IF
END IF

Ptr=Ptr+L3         !- move to next section of Mesage
Ptr2=Ptr2+L3       !- move to next section of MessOut

!=======================================================================
! BUFR section 4
!=======================================================================

! Find length of section 4 & copy it from encoded to output message

IBEFOR=0
L4=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
MessOut(Ptr2:Ptr2+L4-1)=Mesage(Ptr:Ptr+L4-1)

Ptr2=Ptr2+L4       !- move to next section of MessOut

!=======================================================================
! BUFR section 5
!=======================================================================

MessOut(Ptr2:Ptr2+3)=SEVENS

!=======================================================================
! Set total length of BUFR message if user wants edition 2 or more
!=======================================================================

L=8+L1+L2+L3+L4+4
IF (Edition >= 2 .OR. Edition == -99) THEN
  MessOut(5:5)=CHAR(L/65536)
  MessOut(6:6)=CHAR(MOD(L/256,256))
  MessOut(7:7)=CHAR(MOD(L,256))
END IF

IF (L <= LEN(Mesage)) THEN
  Mesage(1:L)=MessOut(1:L)
ELSE
  PRINT *,'ENBUFV4 could not fit message into string provided'
  Nd=0
END IF

RETURN
END SUBROUTINE enbufv4
