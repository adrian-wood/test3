      SUBROUTINE CREX(STRING,DSPLAY,MAXD,MAXV,ND,
     &                IRC,DESCR,VALUES,NAMES)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! PROGRAM       : CREX                                                !
!                                                                     !
! PURPOSE       : decode a report from a CREX bulletin                !
!                  (one report only, not the whole bulletin)          !
!                                                                     !
! DESCRIPTION   : goes past preliminary groups and puts descriptors   !
!                 in an integer form to pass to CREXDAT, which will   !
!                 decode the data section.                            !
!                                                                     !
! DATA TYPE(S)  : any                                                 !
!                                                                     !
! CALLED BY     : user                                                !
!                                                                     !
! CALLS         : CREXVAL (function), CREXDAT                         !
!                                                                     !
! PARAMETERS    : (1) CREX message to be decoded                   (I)!
!                 (2) flag set if display required                 (I)!
!                 (3) dimension of descriptor array                (I)!
!                 (4) dimension of value array                     (I)!
!                 (5) number of descriptors (input as array size) (I/O)
!                 (6) return code                                 (I/O)
!                      (input: 0 at start of message, 4 for more)     !
!                      (output: 0 if no more, 4 if more to come)      !
!                 (7) descriptors (expanded if necessary, in       (O)!
!                      16-bit form like BUFR descriptors, with     !1.2
!                      copy of unexpanded descriptors at end          !
!                      of array in reverse order)                     !
!                 (8) corresponding values (one report only)       (O)!
!                 (9) any character values (pointers in VALUES)    (O)!
!                                                                     !
! Revision Info:                                                      !
! $Revision: 1$                                                        !
! $Date: 30/01/2006 20:21:53$
! $Source: /home/us0400/mdb/op/lib/source/RCS/crex.f,v $
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:53    Sheila Needham  
! $
! Revision 1.2  2004/04/05 11:08:56  usmdb
! 19 April 2004    C Long
! 1.2  Change to 16-bit descriptors as in BUFR
!
! Revision 1.1  2003/05/02 14:36:24  usmdb
! Initial revision
!
!                                                                     !
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.         !
!                                                                     !
! Met Office, United Kingdom                                          !
!                                                                     !
! The use, duplication and disclosure of this code is strictly        !
! prohibited without the permission of The Meteorological Database    !
! Team at the above address.                                          !
!                                                                     !
! ---------------------------------------------------------------------
      CHARACTER STRING*(*)  ! argument 1
      CHARACTER NAMES*(*)   ! argument 9
      REAL VALUES(*)        ! argument 8
      INTEGER DESCR(*)      ! argument 7
      INTEGER ND            ! argument 5
      INTEGER MAXD          ! argument 3
      INTEGER MAXV          ! argument 4
      LOGICAL DSPLAY        ! argument 2
      LOGICAL CHECK_DIGIT   ! set if check digits before values

      CHARACTER*1 F         ! letter at start of descriptor
      INTEGER XX            ! X from descriptor
      INTEGER YYY           ! Y from descriptor
      INTEGER NBEFOR        ! pointer to current group in STRING
!                              (see CREXVAL for precise description)
      INTEGER IRC           ! return code (see header), argument 6
      INTEGER TT            ! table number
      INTEGER EE            ! table edition
      INTEGER VV            ! table version
      INTEGER AAA           ! data type
      INTEGER CREXVAL       ! function to get value from data section
      INTEGER Z             ! dummy varaible for CREXVAL result
      INTEGER ENDESC        ! end of descriptor section
      INTEGER ENDATA        ! end of data section

      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET/.FALSE./

      SAVE

      IF (.NOT. HEADSET) THEN
        HEAD='$RCSfile: crex.f,v $ '//
     &   '$Revision: 1$ $Date: 30/01/2006 20:21:53$ '
        HEADSET = .TRUE.
      ENDIF

! Decode preliminary groups Ttteevv (table etc) and Aaaa (data type)

      IF (IRC.EQ.0) THEN
        CHECK_DIGIT=.FALSE.              ! will be true if E ends sec.1
        NBEFOR=7                         ! past 'CREX++ '
        IT=CREXVAL(STRING,1,NBEFOR)      ! PAST 'T'
        TT=CREXVAL(STRING,2,NBEFOR)      ! tt: table
        EE=CREXVAL(STRING,2,NBEFOR)      ! ee: edition
        VV=CREXVAL(STRING,2,NBEFOR)      ! vv: table version

        IA=CREXVAL(STRING,1,NBEFOR)      ! PAST 'A'
        AAA=CREXVAL(STRING,3,NBEFOR)     ! aaa: data type

! Put descriptors in 16-bit BUFR form F*16384+XX*256+YYY, allowing
! for negative YYY in scale change by adding 128 to YYY in 202YYY.
! Find second ++ (skipping ++ after CREX) as end of descriptors.

        ENDESC=7+INDEX(STRING(8:),'++')
        ND=0                             ! number of descriptors
        DO WHILE (NBEFOR.LT.ENDESC)      ! loop round descriptors
          Z=CREXVAL(STRING,1,NBEFOR)     ! past letter (B,C,D or R)
          F=STRING(NBEFOR-1:NBEFOR-1)    ! keep that letter
          IF (F.EQ.'E') THEN             ! if E at end of section,
            CHECK_DIGIT=.TRUE.           ! check digits in data section
          ELSE
            XX=CREXVAL(STRING,2,NBEFOR)  ! class (if letter is B)
            YYY=CREXVAL(STRING,3,NBEFOR) ! element (if letter is B)

            ND=ND+1                      ! one more descriptor
            IF (STRING(NBEFOR-6:NBEFOR-6).EQ.'B') THEN      ! element
              DESCR(ND)=XX*256+YYY                                  !1.2
            ELSE IF (STRING(NBEFOR-6:NBEFOR-6).EQ.'R') THEN ! replicate
              DESCR(ND)=16384+XX*256+YYY                            !1.2
            ELSE IF (STRING(NBEFOR-6:NBEFOR-6).EQ.'C') THEN ! operator
              DESCR(ND)=32768+XX*256+YYY                            !1.2
              IF (XX.EQ.2) DESCR(ND)=DESCR(ND)+128          ! Y+128
            ELSE IF (STRING(NBEFOR-6:NBEFOR-6).EQ.'D') THEN ! sequence
              DESCR(ND)=49152+XX*256+YYY                            !1.2
            ELSE
              ND=ND-1                    ! one less descriptor
              print *,'descriptor not B,C,D,R',STRING(NBEFOR-6:NBEFOR-6)
              IRC=8
              RETURN
            ENDIF

! Keep copy of sequence (in reverse order) at end of descriptor array
! so that it can be expanded again if there is more than one report.
! MSGND is the number of descriptors in the message (ND may change).

            DESCR(MAXD-ND+1)=DESCR(ND)
          ENDIF
        ENDDO
        MSGND=ND

! Find end of data section.

        ENDATA=INDEX(STRING(ENDESC+2:),'++')
        IF (ENDATA.EQ.0) RETURN
        ENDATA=ENDESC+ENDATA+1

! Move pointer to next group (start of report) & find + at end

        MOVENBEFOR=CREXVAL(STRING,0,NBEFOR)
        IXPLUS=INDEX(STRING(NBEFOR:),'+')

! If not start of message, reset descriptors & delimit next report

      ELSE
        ND=MSGND
        DO I=1,ND
          DESCR(I)=DESCR(MAXD-I+1)
        ENDDO
        MOVENBEFOR=CREXVAL(STRING,0,NBEFOR)
        IXPLUS=INDEX(STRING(NBEFOR:),'+')
      ENDIF

! Get values from data section.

      CALL CREXDAT(DESCR,STRING(NBEFOR:NBEFOR+IXPLUS-1),VALUES,NAMES,ND,
     &             DSPLAY,MAXD-MSGND,MAXV,CHECK_DIGIT,IRC)

! Move pointer past + at end of report, returning with IRC=0 if no more

      NBEFOR=NBEFOR+IXPLUS
      IRC=4
      IF (NBEFOR.GE.ENDATA) IRC=0
      RETURN
      END
