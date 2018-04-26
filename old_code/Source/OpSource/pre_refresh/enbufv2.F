      SUBROUTINE ENBUFV2(Descr,Values,Nd,Nelem,Nobs,Names,Datime,
     &                   Mesage,Cmpres,L,Edition,MasterTable,
     &                   OrigCentre,DataType,DataSubType,VerMasTab,
     &                   VerLocTab,ExtraSect1,CharSect1,ExtraSect2,
     &                   CharSect2,Sect3Type)

!-----------------------------------------------------------------------
!
! ROUTINE        : ENBUFV2
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
!  7) Datime(5)    integer   (ip)    - year, month, day, hour, minute
!  8) Mesage       char*(*)  (op)    - string for BUFR message
!  9) Cmpres       logical   (ip)    - TRUE if compression wanted
! 10) L            integer   (op)    - length of BUFR message
! 11) Edition      integer   (ip)    - Edition No. (sect. 1)
!                                      code -99 for default (now 3)
! 12) MasterTable  integer   (ip)    - BUFR Master table (sect. 1)
!                                      code -99 for default (=0)
! 13) OrigCentre   integer   (ip)    - Originating Centre (sect. 1)
!                                      code -99 for default (=74)
! 14) DataType     integer   (ip)    - Data category type (sect. 1)
!                                      code -99 for default (=255)
! 15) DataSubType  integer   (ip)    - Data category subtype (sect. 1)
!                                      code -99 for default (=0)
! 16) VerMasTab    integer   (ip)    - Version no. of master tables (s.1)
!                                      code -99 for default (now 13)
! 17) VerLocTab    integer   (ip)    - Version no. of local tables (s.1)
!                                      code -99 for default (=0)
! 18) ExtraSect1   logical   (ip)    - code TRUE if there is extra data
!                                      to be added to the end of
!                                      section 1. If so, the data in
!                                      CharSect1 will be added.
! 19) CharSect1    char*(*)  (ip)    - Extra data to add to end of
!                                      section 1.
! 20) ExtraSect2   logical   (ip)    - code TRUE if there is data to
!                                      put in section 2. If so, the
!                                      data in CharSect2 will be added.
! 21) CharSect2    char*(*)  (ip)    - data to put in section 2.
! 22) Sect3Type    integer   (ip)    - section 3 byte 7 (type of data)
!                                      code 1 for observed, 0 for other
!                                      code -99 for default (=1)
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 01/02/2008 15:27:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/enbufv2.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         01/02/2008 15:27:49    Stan Kellett    fixed
!        comment which stated version number was currently 11 when in fact we
!       are updating to 13. It was missed the last change as well.
!  3    Met_DB_Project 1.2         01/02/2008 14:48:22    Stan Kellett
!       Updated Mas table version from 12 to 13 for WMO November 2007 update.
!  2    Met_DB_Project 1.1         30/06/2006 10:28:18    Stan Kellett
!       changed MAster table version number from 11 to 12
!  1    Met_DB_Project 1.0         30/01/2006 20:22:13    Sheila Needham  
! $
! Revision 2.2  2004/02/02 12:05:17  usmdb
! 16 Feb 2004     C Long
! 2.2  Combine BUFV2 with ENBUFV2.  Dont change default ar
! guments.
!      Update default edition & version to 3 & 11.
!      Another check for negative value from ICHAR added fo
! r consistency.
!
! Revision 2.1  2002/04/09  11:44:29  11:44:29  usmdb (MetDB account c/o usjh)
! Changed declaration of VALUES from INTEGER to REAL. Removed
! unused variable SEVENS & therefore EB2ASC call - S.Cox
!
! Revision 2.0  2001/03/07 10:19:14  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  1998/07/23 09:06:11  usmdb
! Extra argument to BUFV2 to return the length of the BUFR message.
! Dont calculate the length of the BUFR message in ENBUFV2 anymore as
! this check is in error.
!
! Revision 1.1  97/06/19  13:39:35  13:39:35  uspm (Pat McCormack)
! Initial revision
!
! ----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER    MaxLen
      PARAMETER  (MaxLen=27998)      !- max length of BUFR message.

      INTEGER    Byte7               !- byte 7 of section 3
      INTEGER    CurrentEdtn         !- edition number from ENBUFR
      INTEGER    DataSubType         !- BUFR subtype
      INTEGER    DataType            !- BUFR type
      INTEGER    Datime(5)           !- date/time array
      INTEGER    Descr(*)            !- array of descriptors
      INTEGER    Edition             !- BUFR edition
      INTEGER    Flopt               !- 1 if there is a section 2
      INTEGER    IBEFOR              !- bits before value in string !2.1
      INTEGER    L                   !- length of BUFR message
      INTEGER    Lin                 !- length of sect 1 or 2 in Mesage
      INTEGER    L1,L2,L3,L4         !- lengths of sections in MessOut
      INTEGER    MasterTable         !- BUFR master table number
      INTEGER    Nd                  !- number of descriptors
      INTEGER    Nelem               !- number of elements
      INTEGER    Nobs                !- number of observations
      INTEGER    OptPtr              !- pointer to optional section byte
      INTEGER    OrigCentre          !- originating centre
      INTEGER    Ptr                 !- pointer to Mesage
      INTEGER    Ptr2                !- pointer to MessOut
      INTEGER    Sect3Type           !- type of data (section 3)
      INTEGER    VALUE               !- function
      INTEGER    VerMasTab           !- version no. of master tables
      INTEGER    VerLocTab           !- version no. of local tables

      LOGICAL    Cmpres              !- TRUE for compression
      LOGICAL    ExtraSect1          !- TRUE if extra sect 1 data
      LOGICAL    ExtraSect2          !- TRUE if sect 2 to be added
      LOGICAL    First               !- TRUE if first call

      REAL       Values(Nobs,Nelem)  !- values to encode            !2.1

      CHARACTER*(*)       CharSect1  !- section 1 extra data
      CHARACTER*(*)       CharSect2  !- section 2 data
      CHARACTER*132       Head       !- revision information
      CHARACTER*(MaxLen)  MessOut    !- BUFR message
      CHARACTER*(*)       Names      !- strings to encode
      CHARACTER*4         BUFR       !- character string 'BUFR'
      CHARACTER*(*)       Mesage     !- input string for BUFR message
      CHARACTER*4         SEVENS     !- character string '7777'

! SAVE statement to ensure all variables are still set on next call

      SAVE

      DATA First/.TRUE./           !- TRUE for first call to ENBUFV2

      COMMON /EBV2/MessOut

! First time only, Initialise BUFR & SEVENS. Set Revision info.

      IF (First) THEN
        First=.FALSE.
        BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)
        SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)
        Head='$RCSfile: enbufv2.F,v $ ' //
     &       '$Revision: 4$ $Date: 01/02/2008 15:27:49$'
      ENDIF

! Call ENBUFR to code BUFR message

      CALL ENBUFR(Descr,Values,Nd,Nelem,Nobs,Names,Datime,Mesage,
     &            Cmpres,L)

! Check that ENBUFR has made a BUFR message

      Ptr=INDEX(Mesage,BUFR)

      IF (ND.EQ.0 .OR. Ptr.EQ.0 .OR. INDEX(Mesage,SEVENS).EQ.0) THEN
        PRINT *,'In ENBUFV2: message not made by ENBUFR'
        ND=0
        RETURN
      ENDIF

      Ptr2=1                       !- pointer in output message

!=======================================================================
! BUFR section 0
!=======================================================================

      MessOut(1:4)=BUFR

! If the user wants edition 2 or more (which is the default),
! leave room for the total length at the start
! & set the edition in the last byte of those four.

      IF (Edition.GE.2) THEN
        MessOut(8:8)=CHAR(Edition)
        Ptr2=Ptr2+8
      ELSE IF (Edition.EQ.-99) THEN
        MessOut(8:8)=CHAR(3)                                        !2.2
        Ptr2=Ptr2+8
      ELSE
        Ptr2=Ptr2+4
      ENDIF

! Move to BUFR section 1 of the encoded message

      CurrentEdtn=ICHAR(Mesage(Ptr+7:Ptr+7))
      IF (CurrentEdtn.LT.0) CurrentEdtn=CurrentEdtn+256

      Ptr=Ptr+4
      IF (CurrentEdtn.GE.2) Ptr=Ptr+4

!=======================================================================
! BUFR section 1
!=======================================================================

! Find length of section 1 & copy that section from encoded to output
! message. See if the encoded message has an optional section 2.

      IBEFOR=0
      Lin=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
      L1=Lin

      MessOut(Ptr2:Ptr2+Lin-1)=Mesage(Ptr:Ptr+Lin-1)

      Flopt=ICHAR(Mesage(Ptr+7:Ptr+7))
      IF (Flopt.LT.0) Flopt=Flopt+256
      OptPtr=Ptr2+7

! Update section 1 bytes if necessary.
! In edition 0 or 1 the byte after the length of section 1 is the
! edition number, in later editions it is the master table number,
! - so the edition number (set above for edition 2 or more) is
! always in the 8th byte of a message.

      IF (Edition.EQ.0 .OR. Edition.EQ.1) THEN
        MessOut(Ptr2+3:Ptr2+3)=CHAR(Edition)
      ELSE
        IF (MasterTable.NE.-99) MessOut(Ptr2+3:Ptr2+3)=CHAR(MasterTable)
      ENDIF

! Default originating centre is 74 (UK Met Office)

      IF (OrigCentre.NE.-99) THEN
        MessOut(Ptr2+4:Ptr2+4)=CHAR(OrigCentre/256)
        MessOut(Ptr2+5:Ptr2+5)=CHAR(MOD(OrigCentre,256))
      ELSE
        MessOut(Ptr2+4:Ptr2+4)=CHAR(0)
        MessOut(Ptr2+5:Ptr2+5)=CHAR(74)
      ENDIF

      IF (DataType.NE.-99)    MessOut(Ptr2+8:Ptr2+8)=CHAR(DataType)
      IF (DataSubType.NE.-99) MessOut(Ptr2+9:Ptr2+9)=CHAR(DataSubType)
      IF (VerMasTab.NE.-99)   MessOut(Ptr2+10:Ptr2+10)=CHAR(VerMasTab)
      IF (VerLocTab.NE.-99)   MessOut(Ptr2+11:Ptr2+11)=CHAR(VerLocTab)

! Default subtype cant be zero ("Surface data - land" in Table A.
! (But 255 is "for local use" (with subtype) rather than missing!)
! Default table version is latest (see Manual on Codes for number)

      IF (DataType.EQ.-99)    MessOut(Ptr2+8:Ptr2+8)=CHAR(255)
      IF (DataSubType.EQ.-99) MessOut(Ptr2+9:Ptr2+9)=CHAR(0)
      IF (VerMasTab.EQ.-99)   MessOut(Ptr2+10:Ptr2+10)=CHAR(13)   !ST.3

! If the user wants to add extra data to section 1, check to see if
! there is already extra data (Lin.GT.18) & if so ignore users data.
! If not, calculate the length of the new section 1 and add the
! extra data to it. Check to see if there is an odd number of bytes and
! if so, pad with a zero and update the new length. Finally, put the
! length into section 1 bytes 1-3 and move to next section.

      IF (ExtraSect1) THEN
        IF (Lin.GT.18) THEN
          PRINT *,'ENBUFV2 found extra data already in section 1'
          PRINT *,' so input extra data for section 1 ignored'
        ELSE
          L1=17+LEN(CharSect1)

#if defined (MVS)

          CALL EB2ASC(LEN(CharSect1),CharSect1)
#endif
          MessOut(Ptr2+17:Ptr2+L1-1)=CharSect1
          IF (MOD(L1,2).NE.0) THEN
            MessOut(Ptr2+L1:Ptr2+L1)=CHAR(0)
            L1=L1+1
          ENDIF
          MessOut(Ptr2:Ptr2+2)=CHAR(0)//CHAR(L1/256)//CHAR(MOD(L1,256))
        ENDIF
      ENDIF

      Ptr=Ptr+Lin        !- move to next section of Mesage
      Ptr2=Ptr2+L1       !- move to next section of MessOut

!=======================================================================
! BUFR section 2
!=======================================================================

! If there is already an optional section 2, then find its length and
! copy it from the encoded message to section 2 of the output message.

      L2=0
      Lin=0
      IF (Flopt.GE.128) THEN
        IBEFOR=0
        Lin=VALUE(Mesage(Ptr:Ptr+2),IBEFOR,24)
        L2=Lin

        MessOut(Ptr2:Ptr2+Lin-1)=Mesage(Ptr:Ptr+Lin-1)

        IF (ExtraSect2) THEN
          PRINT *,'ENBUFV2 found extra data already in section 2'
          PRINT *,' so input section 2 ignored'
        ENDIF

! If the user wants to add extra data to section 2 and there is not
! already a section 2, calculate the length of the new section 2, and
! add the extra data to it. Check to see if there is an odd number of
! bytes and if so, pad with a zero and update the new length. Finally,
! put the length into section 1 bytes 1-3, set bit 1 of byte 8 in BUFR
! section 1 and move to next section.

      ELSE IF (ExtraSect2) THEN
        MessOut(Ptr2+3:Ptr2+3)=CHAR(0)
        L2=4+LEN(CharSect2)

#if defined (MVS)

        CALL EB2ASC(LEN(CharSect2),CharSect2)
#endif
        MessOut(Ptr2+4:Ptr2+L2-1)=CharSect2
        IF (MOD(L2,2).NE.0) THEN
          MessOut(Ptr2+L2:Ptr2+L2)=CHAR(0)
          L2=L2+1
        ENDIF
        MessOut(Ptr2:Ptr2+2)=CHAR(0)//CHAR(L2/256)//CHAR(MOD(L2,256))
        MessOut(OptPtr:OptPtr)=CHAR(128)
      ENDIF

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

      IF (Sect3Type.NE.-99) THEN
        Byte7=ICHAR(Mesage(Ptr+6:Ptr+6))
        IF (Byte7.LT.0) Byte7=Byte7+256                             !2.2

        IF (Byte7.GE.128 .AND. Sect3Type.EQ.0) THEN
          MessOut(Ptr2+6:Ptr2+6)=CHAR(Byte7-128)
        ELSE IF (Byte7.LT.128 .AND. Sect3Type.EQ.1) THEN
          MessOut(Ptr2+6:Ptr2+6)=CHAR(Byte7+128)
        ENDIF
      ENDIF

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
      IF (Edition.GE.2 .OR. Edition.EQ.-99) THEN                    !2.2
        MessOut(5:5)=CHAR(L/65536)
        MessOut(6:6)=CHAR(MOD(L/256,256))
        MessOut(7:7)=CHAR(MOD(L,256))
      ENDIF

      IF (L.LE.LEN(Mesage)) THEN
        Mesage(1:L)=MessOut(1:L)
      ELSE
        PRINT *,'ENBUFV2 could not fit message into string provided'
        Nd=0
      ENDIF

      RETURN
      END
