       SUBROUTINE TAFSECT(OB,IN,REXP,SECTION,RC,FACT)

!-----------------------------------------------------------------------
!
!  PROGRAM     : TAFSECT
!
!  PURPOSE     : To see if this TAF group starts a change section.
!                If so, store the period and a flag corresponding
!                to the keyword found.
!
!  DESCRIPTION : Because some sequences occur in practice with or
!                without spaces (between FM & time, time & Z...)
!                and because keywords are sometimes split between
!                lines.
!
!  CALLED BY   : TAFEXP
!
!  CALLS       : IVALUE
!
!  PARAMETERS  : 1 report being expanded                 (i)
!                2 pointer to current group in report    (i/o)
!                3 expansion array                       (i/o)
!                4 Taf format indicator                  (i)
!                5 section number                        (i/o)
!                6 return code:                          (o)
!                   RC=1: new section
!                   RC=0: not new section
!                   RC=-1: may be new section, but error in e.g. time
!                7  Expansion array factor                (i)
!
! REVISION INFO :
!
! $Revision: 9$
! $Date: 17/11/2008 10:22:41$
! $Author: Richard Weedon$
! $Folder: pre_refresh$
! $Workfile: tafsect.f$
!
! CHANGE RECORD :
!
! $Log:
!  9    Met_DB_Project 1.8         17/11/2008 10:22:41    Richard Weedon
!       tafsect.f updated with comments on latest change (ver 1.5)
!  8    Met_DB_Project 1.7         07/11/2008 16:22:36    Richard Weedon  Minor
!        modifications
!  7    Met_DB_Project 1.6         07/11/2008 11:39:05    Richard Weedon
!       corrections made
!  6    Met_DB_Project 1.5         07/11/2008 11:30:15    Richard Weedon
!       corrections made
!  5    Met_DB_Project 1.4         07/08/2008 11:47:34    John Norton
!       Updated with header statements
!  4    Met_DB_Project 1.3         04/08/2008 12:15:30    John Norton
!       Updates after reviewing changes
!  3    Met_DB_Project 1.2         10/06/2008 14:27:50    Richard Weedon  Rexp
!       array indices aletered
!  2    Met_DB_Project 1.1         06/06/2008 16:53:18    Richard Weedon
!       Alternate date time format introduced. Parameter added to indicate TAF
!        format
!  1    Met_DB_Project 1.0         30/01/2006 20:25:16    Sheila Needham  
! $
!  Work Log
!  Revision 1.5
!  7th Nov 2008 Testing by the Taf Verification team found a number of faults.
!               The OFFSET value in TAFEXP.f and TAFSECT.f had been reset to 
!                39 to accomadate the increase in the no of change sections.
!               A number of the array indices were incorrect.
!               The following changes were made as a result -
!               The varaiable FACT added in the call from TAFEXP.   
!               The indices of IVALUE corrected.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

       IMPLICIT NONE

       CHARACTER*(*) OB          ! report

       CHARACTER*91  INDFIVE     !
       CHARACTER*16  INDFOUR
       CHARACTER*16  INDTWO



       CHARACTER*20  GP          ! text from OB(IN:) without spaces
       CHARACTER*20  CHARR        ! CHARR RETURN GRP MTRGRP

       REAL          REXP(*)     ! expansion

       INTEGER       INDEXIND

       INTEGER       GPPRES
       INTEGER       GRPLEN      ! LENGTH OF THE GROUP - MTRGRP
       INTEGER       DAY,DAY2    ! Day Period
       INTEGER       IN          ! pointer to group to be checked
       INTEGER       INR         ! holding var for IN
       INTEGER       INPUT(20)   ! subscript mapping from OB(:IN) to GP
       INTEGER       SECTION     ! zero, then number of change section
       INTEGER       SWITCH      ! Taf Format indicator 1=new 0=old
       INTEGER       FLAG        ! corresponding to keyword found
       INTEGER       PROB        ! set if PROB30 or PROB40 found
       INTEGER       OFFSET      ! for change section subscripts
       INTEGER       HOUR,HOUR2  ! period
       INTEGER       MINUTE      ! may be minute after hour if FM or TL
       INTEGER       IVALUE      ! to get integer from figure(s)
       INTEGER       I           ! pointer to character in OB(IN:) or G P
       INTEGER       INGP        ! no. of chars in GP (may be <LEN(GP))
       INTEGER       RC          ! return code
       INTEGER       FACT        ! Expansion array factor
       INTEGER       NCHAR       ! NCHAR RETURNED FROM MTRLOC

       LOGICAL       LREPFL      ! RETURN CODE FOR MTRLOC


       LOGICAL HEADSET
       CHARACTER*132 HEAD
       DATA HEADSET/.FALSE./
     
       IF (.NOT.HEADSET) THEN
         HEAD='$Workfile: tafsect.f$ ' //
     & '$Revision: 9$ $Date: 17/11/2008 10:22:41$'
         HEADSET=.TRUE.
       ENDIF


 ! Intialise Variables
       I=0
       RC=0
       INGP=0
       GP=' '
       PROB=0
       FLAG=0
       I=1
       INGP=LEN(OB)
       INDEXIND=-1
       INR=IN
! Set the search strings. These contain permutations of the
! change field indicators. Index searches are made against them
!         INDFIVE - TEMPO, BECMG, GRADU, RAPID
!         INDFOUR - PROB , TEND
!         INDTWO - FM, TL

       INDFIVE(1:25)='TENPOTEMPOTNEPOTEPMOTMNPO'
       INDFIVE(26:55)='BECMGBECNGBEMCGGRADUGARDUGDARU'
       INDFIVE(56:85)='RAPIDRPAIDRIPADINTERIRNTEITNER'
       INDFOUR='FMTLPROBTENDTNEDTDNE'


      I=IN
      CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
      GP=OB(I:I+GRPLEN-1)


! Index search against the change field indicators

      IF ( GRPLEN.EQ.5) THEN
         INDEXIND=INDEX(INDFIVE,GP(1:5))
         IF (INDEXIND.LE.25) FLAG=16
         IF (INDEXIND.LE.40.AND.INDEXIND.GE.26) FLAG=32
         IF (INDEXIND.LE.55.AND.INDEXIND.GE.41) FLAG=4
         IF (INDEXIND.LE.70.AND.INDEXIND.GE.56) FLAG=8
         IF (INDEXIND.LE.85.AND.INDEXIND.GE.71) FLAG=64
      ENDIF

      IF (GRPLEN.EQ.8.OR.GRPLEN.EQ.6) THEN
       INDEXIND=INDEX(INDFOUR,GP(1:2))

       IF (INDEXIND.GT.0) THEN
         IF (INDEXIND.EQ.5) FLAG=1
         IF (INDEXIND.GE.9) FLAG=128
         IF (INDEXIND.EQ.1) FLAG=512
         IF (INDEXIND.EQ.3) FLAG=0
       ENDIF

      ENDIF


! A return value of greater than or equal to 1
! signifies the presence of a change field indicator

      IF (INDEXIND.GT.0) THEN   ! NEW SECTION FOUND 1

! increment the section number and establsh the offset. The
! Return code is set to indicate the presence of a new
! change section.

        SECTION=SECTION+1
        OFFSET=(SECTION-1)*FACT    ! 1.5
        RC=1

! Set the values of the exp array to -99999999999

        DO I=66,105
          REXP(I+OFFSET)=-9999999.
        ENDDO

! The PROB indicator will mean that a Prob Value follows.
! This in turn may be followed by the TEMPO field or a
! field date time .
   !
        IF (FLAG.EQ.1.OR.FLAG.EQ.128) THEN

          PROB=IVALUE(GP(5:6))

          IF (PROB.LT.0) PROB=IVALUE(GP(5:5))*10

          IF (PROB.GT.0) THEN
            REXP(69+OFFSET)=PROB
            REXP(66+OFFSET)=FLAG
            I=IN

! Check for the date time group after prob.The following
! checks for date / time groups in both the old and new format

            CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
            GP=OB(I:I+GRPLEN-1)
            CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)
            IF (GRPLEN.EQ.5) THEN
            INDEXIND=INDEX(INDFIVE,GP(1:5))
            ELSE
            INDEXIND=-1
            ENDIF
            IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
              REXP(67+OFFSET)=IVALUE(GP(3:4))
              REXP(68+OFFSET)=IVALUE(GP(8:9))
              REXP(96+OFFSET)=IVALUE(GP(1:2))
              REXP(97+OFFSET)=IVALUE(GP(6:7))
              RC=1
            ELSE IF (GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN'
     &           .AND.GP(1:2).LT.'31'.AND.GP(3:4).LE.'24'
     &           .AND.GP(5:6).LE.'24') THEN
              REXP(67+OFFSET)=IVALUE(GP(3:4))
              REXP(68+OFFSET)=IVALUE(GP(5:6))
              REXP(96+OFFSET)=IVALUE(GP(1:2))
              RC=1
            ELSE IF (GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN'
     &        .AND.GP(1:2).LE.'24'.AND.GP(3:4).LE.'24') THEN
              REXP(67+OFFSET)=IVALUE(GP(1:2))
              REXP(68+OFFSET)=IVALUE(GP(3:4))
              RC=1
            ELSE IF (GRPLEN.EQ.5.AND.INDEXIND.GT.0
     &               .AND.INDEXIND.LT.25) THEN
              FLAG=FLAG+16
            ELSE
! A date time group may not be associated with the PROB indicator.
! This being the case return of -1 will be issued
              RC=-1
            ENDIF

          ENDIF
! If a PROB indicator has been found but the value associated with it
! is unsatisfactory return a value of -1

        IF (PROB.EQ.0) THEN
          RC=-1
        ENDIF

      ENDIF

!Check for the presence of FM as the change indicator. If this is the
! case the date time will not be a seperate group.

! FM with a date time group in the older format

        IF (FLAG.EQ.512.OR.FLAG.EQ.0.AND.GRPLEN.EQ.6
     &      .AND.CHARR(1:6).EQ.'YYNNNN') THEN
        REXP(66+OFFSET)=FLAG
        REXP(67+OFFSET)=IVALUE(GP(3:4))
        REXP(100+OFFSET)=IVALUE(GP(5:6))    ! 1.5
      ENDIF

! FM with a date time group in the newer format

       IF (FLAG.EQ.512.OR.FLAG.EQ.0.AND.GRPLEN.EQ.8
     &    .AND.CHARR(1:8).EQ.'YYNNNNNN') THEN
           REXP(66+OFFSET)=FLAG
           REXP(96+OFFSET)=IVALUE(GP(3:4))   !1.5
           REXP(67+OFFSET)=IVALUE(GP(5:6))   !1.5
           REXP(100+OFFSET)=IVALUE(GP(7:8))  !1.5 
        ENDIF

! Check for the presence of either TEMPO, BECMG, GRADU , RAPID
! or INTER as the change indicator

        IF ((FLAG.EQ.32.OR.FLAG.EQ.4.OR.FLAG.EQ.8.OR.
     &     FLAG.EQ.64.OR.FLAG.EQ.16.OR.FLAG.EQ.17.).AND.
     &     GRPLEN.EQ.5) THEN

! assuming one of the above is found advance to the next
! group which should be the validity period
        REXP(66+OFFSET)=FLAG
        I=IN
        CALL MTRLOC(INGP,OB,IN,GRPLEN,LREPFL)
        GP=OB(I:I+GRPLEN-1)
        CALL MTRGRP(GP,GRPLEN,NCHAR,CHARR)

! check the format of the validity period

        IF (GRPLEN.EQ.9.AND.CHARR(1:9).EQ.'NNNN/NNNN') THEN
          REXP(67+OFFSET)=IVALUE(GP(3:4))
          REXP(68+OFFSET)=IVALUE(GP(8:9))
          REXP(96+OFFSET)=IVALUE(GP(1:2))
          REXP(97+OFFSET)=IVALUE(GP(6:7))
          RC=1
        ELSE IF (GRPLEN.EQ.6.AND.CHARR(1:6).EQ.'NNNNNN') THEN
          REXP(67+OFFSET)=IVALUE(GP(3:4))
          REXP(68+OFFSET)=IVALUE(GP(5:6))
          REXP(96+OFFSET)=IVALUE(GP(1:2))
          RC=1
        ELSE IF (GRPLEN.EQ.4.AND.CHARR(1:4).EQ.'NNNN') THEN
          REXP(67+OFFSET)=IVALUE(GP(1:2))
          REXP(68+OFFSET)=IVALUE(GP(3:4))
          RC=1
        ELSE
! if the change indicator is not followed by a date / time group
! return a value of -1
          RC=-1
        ENDIF
      ENDIF

      ELSE
! where the group is found not to be a change section
!  return an indicator value of 0
        RC=0
        IN=INR
      ENDIF

      RETURN
      END
