      SUBROUTINE CREXDEC(STRING,DESCR,ND,VALUES,NAMES,
     &                   DSPLAY,MAXD,MAXV,IRC)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! PROGRAM       : CREXDEC                                             !
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
!                 (2) descriptors (expanded if necessary, in       (O)!
!                      integer form like readable BUFR descriptors)   !
!                 (3) number of descriptors (input as array size) (I/O)
!                 (4) corresponding values (one report only)       (O)!
!                 (5) any character values (pointers in value array)  !
!                 (6) flag set if display required                 (I)!
!                 (7) dimension of descriptor array                (I)!
!                 (8) dimension of value array                     (I)!
!                 (9) return code                                 (I/O)
!                      (output: 0 if no more, 4 if more to come)      !
!                      (input: 0 at start of message, 4 for more)     !
!                                                                     !                                                                     !
! Revision Info:                                                      !
! $Revision: 1$                                                        !
! $Date: 11/10/2006 11:56:55$                                                            !
! $Source: /home/us0400/mdb/op/lib/other/MDB.CREX.SRCE/RCS/crexdec.f,v $ 
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         11/10/2006 11:56:55    Kudsia Gwangwaa 
! $
! Revision 1.1  2002/10/22 09:54:34  usmdb
! Initial revision
!
!                                                                     !
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.         !
!                                                                     !
! Met Office, United Kingdom                                          !
!                                                                     !
! The use, duplication and disclosure of this code is strictly        !
! prohibited without the permission of The Meteorological Database    !
! Team at the above address.                                          !
!                                                                     !
! ---------------------------------------------------------------------
      CHARACTER STRING*(*),NAMES*(*)
      REAL VALUES(*)
      INTEGER DESCR(*)
      INTEGER ND,MAXD,MAXV
      LOGICAL DSPLAY
      LOGICAL CHECK_DIGIT           ! set if check digits before values

      CHARACTER*1 F                 ! letter at start of descriptor
      INTEGER XX,YYY                ! numbers in descriptor
      INTEGER PTR,IRC
      INTEGER TT,EE,VV,AAA
      INTEGER CREXVAL,Z
      INTEGER ENDESC, ENDATA
! ---------------------------------------------------------------------
! Revision Information: 
! ---------------------------------------------------------------------
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET/.FALSE./
      
      SAVE
      
      IF (.NOT. HEADSET) THEN
        HEAD='$RCSfile: crexdec.f,v $ '/
     &   '$Revision: 1$ $Date: 11/10/2006 11:56:55$ '
        HEADSET = .TRUE.
      ENDIF
      


! Decode preliminary groups Ttteevv (table etc) and Aaaa (data type)

      IF (IRC.EQ.0) THEN
        CHECK_DIGIT=.FALSE.         ! no check digit unless E in sect.1
        PTR=6                       ! past 'CREX '
        IT=CREXVAL(STRING,PTR,1)    ! PAST 'T'
        TT=CREXVAL(STRING,PTR,2)    ! tt: table
        EE=CREXVAL(STRING,PTR,2)    ! ee: edition
        VV=CREXVAL(STRING,PTR,2)    ! vv: table version

        IA=CREXVAL(STRING,PTR,1)    ! PAST 'A'
        AAA=CREXVAL(STRING,PTR,3)   ! aaa: data type

! Put descriptors in integer form F*100000+XX*1000+YYY, allowing for
! negative YYY in scale change by adding 500 to YYY in 202YYY.

        ND=0                        ! number of descriptors
        ENDESC=INDEX(STRING,'++')
        DO WHILE (PTR.LT.ENDESC)
          Z=CREXVAL(STRING,PTR,1)   ! point past letter (B,C,D or R)
          F=STRING(PTR-1:PTR-1)     ! keep that letter
          IF (F.EQ.'E') THEN        ! if E at end of section,
            CHECK_DIGIT=.TRUE.      ! check digits used in data section
          ELSE
            XX=CREXVAL(STRING,PTR,2) ! class (if letter is B)
            YYY=CREXVAL(STRING,PTR,3) ! element (if letter is B)

            ND=ND+1                 ! one more descriptor
            IF (STRING(PTR-6:PTR-6).EQ.'B') THEN    ! element
              DESCR(ND)=XX*1000+YYY
            ELSE IF (STRING(PTR-6:PTR-6).EQ.'R') THEN ! replication
              DESCR(ND)=100000+XX*1000+YYY
            ELSE IF (STRING(PTR-6:PTR-6).EQ.'C') THEN ! operator
              DESCR(ND)=200000+XX*1000+YYY
              IF (XX.EQ.2) DESCR(ND)=202000+(YYY+500)
            ELSE IF (STRING(PTR-6:PTR-6).EQ.'D') THEN ! sequence
              DESCR(ND)=300000+XX*1000+YYY
            ELSE
              ND=ND-1               ! one less descriptor (reset ND)
              print *,'strange letter in descriptor',string(ptr-6:ptr-6)
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
! Move pointer to next group & look for + at end of each report

        ENDATA=INDEX(STRING(ENDESC+2:),'++')
        IF (ENDATA.EQ.0) RETURN
        ENDATA=ENDESC+ENDATA+1

! Move pointer to start of report & find + at end

        MOVEPTR=CREXVAL(STRING,PTR,0)
        IXPLUS=INDEX(STRING(PTR:),'+')

! If not start of message, reset descriptors & delimit next report

      ELSE
        ND=MSGND
        DO I=1,ND
          DESCR(I)=DESCR(MAXD-I+1)
        ENDDO
        MOVEPTR=CREXVAL(STRING,PTR,0)
        IXPLUS=INDEX(STRING(PTR:),'+')
      ENDIF

      CALL CREXDAT(DESCR,STRING(PTR:PTR+IXPLUS-1),VALUES,NAMES,ND,
     &             DSPLAY,MAXD-MSGND,MAXV,CHECK_DIGIT,IRC)

! Move pointer past + at end of report, returning with IRC=0 if no more

      PTR=PTR+IXPLUS
      IRC=4
      IF (PTR.GE.ENDATA) IRC=0
      RETURN
      END
