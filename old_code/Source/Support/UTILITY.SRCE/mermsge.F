      PROGRAM MERMESGE

      PARAMETER (L1=15)               ! LAT & LONG SUBSCRIPTS
      PARAMETER (L2=16)               ! LAT & LONG SUBSCRIPTS

      INTEGER   DESC(50000)
      INTEGER   USERDESC(50000)                    !S.Cox               00014999
      INTEGER   OCOUNT,DCOUNT,FF,XX,YY             !S.Cox
      INTEGER   BSQ                                !S.Cox
      REAL      OUT(50000)
      REAL      USERVALS(20,2500)                  !S.Cox               00014899
      LOGICAL   CALL_STRING                        !S.Cox
      LOGICAL   DISPLAY_CALL_STRING                !S.Cox
      LOGICAL   DISPLAY_FULL_ASSOC                 !S.Cox
      LOGICAL   FIRST_ASSOC                        !S.Cox
      CHARACTER M*222
      CHARACTER MESS*25000

      DATA FIRST_ASSOC/.TRUE./

      DISPLAY_CALL_STRING = .TRUE.   !- display MetDB call string
      DISPLAY_FULL_ASSOC  = .FALSE.  !- full BUFR decode of assoc data

!-----------------------------------------------------------------------00024199
! read and decode MDB request string at start of assoc data             00024299
!-----------------------------------------------------------------------00024399
                                                                        00024421
      WRITE(6,'(/1X,''-------------------------------------'')')
      WRITE(6,'( 1X,''Assoc file MetDB call string(s)      '')')
      WRITE(6,'( 1X,''-------------------------------------'')')

      CALL_STRING = .TRUE.
      DO WHILE (CALL_STRING)
        CALL MODELB(1,MESS,L,IRC)       ! REQUEST MESSAGE
        BSQ = 0
        IF (ICHAR(MESS(8:8)).GT.1) BSQ=4
        IDESC=ICHAR(MESS(30+BSQ:30+BSQ))*256+ICHAR(MESS(31+BSQ:31+BSQ))
        CALL DESFXY(IDESC,FF,XX,YY)
        IF (FF.EQ.2 .AND. XX.EQ.5) THEN
          CALL_STRING = .TRUE.
          IF (DISPLAY_CALL_STRING) THEN
            WRITE(6,*)
            CALL DEBUFR(DESC,OUT,M,ND,NOB,MESS,.TRUE.)
          ENDIF
        ELSE
          CALL_STRING = .FALSE.
        ENDIF
      ENDDO

!-----------------------------------------------------------------------00024199
! read and BUFR decode assoc data.                                      00024299
!-----------------------------------------------------------------------00024399
                                                                        00024421
      WRITE(6,'(/1X,''-------------------------------------'' )')
      WRITE(6,'( 1X,''Assoc data                           '' )')
      WRITE(6,'( 1X,''-------------------------------------''/)')

      IRC = 0

 20   IF (.NOT.FIRST_ASSOC) THEN
        CALL MODELB(1,MESS,L,IRC)
      ELSE
        FIRST_ASSOC=.FALSE.
      ENDIF

      IF (IRC.EQ.0) THEN

        ND  = 50000
        NOB = 50000

        CALL DEBUFR(DESC,OUT,M,ND,NOB,MESS,DISPLAY_FULL_ASSOC)

!-----------------------------------------------------------------------00024199
! additional section - S.Cox                                            00024299
!-----------------------------------------------------------------------00024399
                                                                        00024421
        IF (DISPLAY_FULL_ASSOC) THEN
          DCOUNT=0                                                          0002
          OCOUNT=0                                                          0002
          DO JJ=1,ND                                                        0002
            CALL DESFXY(DESC(JJ),FF,XX,YY)                                  0002
            USERDESC(JJ)=100000*FF+1000*XX+YY                               0002
            DO II=1,NOB                                                     0002
              OCOUNT=OCOUNT+1                                               0002
              USERVALS(II,JJ)=OUT(OCOUNT)                                   0002
            ENDDO                                                           0002
          ENDDO                                                             0002
          DO II=1,NOB                                                       0002
            WRITE(6,*)
            DCOUNT=0                                                        0002
            DO JJ=1,ND                                                      0002
              IF ((USERDESC(JJ)/100000).EQ.0) THEN                          0002
                DCOUNT=DCOUNT+1                                             0002
                WRITE(6,'(1X,I5.5,2X,I6.6,2X,F21.12)')JJ,                   0002
     &          USERDESC(JJ),USERVALS(II,DCOUNT)                            0002
              ELSE                                                          0002
                WRITE(6,'(1X,I5.5,2X,I6.6)')JJ,USERDESC(JJ)                 0002
              ENDIF                                                         0002
            ENDDO                                                           0002
          ENDDO                                                             0002
        ENDIF
                                                                        00027199
!-----------------------------------------------------------------------00027299
! end of additional section - S.Cox                                     00027399
!-----------------------------------------------------------------------00027499
                                                                        00027599
        DO I=1,NOB
          PRINT *,OUT(NOB*(L1-1)+I),OUT(NOB*(L2-1)+I)
        ENDDO
        GOTO 20
      ENDIF

      STOP
      END
