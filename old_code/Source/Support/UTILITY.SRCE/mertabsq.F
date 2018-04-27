! TO CHECK DESCRIPTORS IN MERGE TABLE AGAINST SEQUENCE IN LIBRARY
! (SEQUENCE IN DATA SET NEEDS READ COMMENTED OUT BY *)
!
      INTEGER SEQ_DESCR(999),TABLE_DESCR(999),LBKGD(999)
      CHARACTER DTEXT*23476
                                                                        01740002
! READ SEQUENCE FROM LIBRARY OR DATA SET, PUT IT IN LOCAL TABLE D       01740002
! & GET IT BACK IN 16-BIT INTEGER FORM.                                 01740002
                                                                        01740002
*     OPEN (1,ACCESS='DIRECT',RECL=?????)                               01700002
*     READ (1,REC=2) DTEXT        ! LOCAL D SEQUENCE FROM SECOND RECORD 01720002
! DIFFERENT I/O TO READ SEQUENCE FROM 80-BYTE RECORDS IN LIBRARY:       01740002
      OPEN (1,FORM='UNFORMATTED')                                       00018100
      L=0                                                               00018200
   20 READ (1,END=21) DTEXT(L*80+1:L*80+80)                             00018300
      L=L+1                                                             00018400
      GO TO 20                                                          00018500
   21 DTEXT(L*80+1:L*80+5)=' END '                                      00018601
*                                                                       00017800
      READ (DTEXT(2:3),'(I2)') LX ! SEQUENCE DESCRIPTOR
      READ (DTEXT(4:6),'(I3)') LY ! AT START OF FIRST LINE
      CALL LOCALD(0,0,I1,I2,DTEXT,'ADD')                                01730002
      CALL LOCALD(LX,LY,SEQ_DESCR,ND,DTEXT,' ')                         01730002
                                                                        01740002
! CONVERT DESCRIPTORS FROM 16 BITS TO FXXYYY, REMOVING OPERATORS        01740002
                                                                        01740002
      NDL=ND                      ! NUMBER OF SEQUENCE DESCRIPTORS
      IL=0
      DO I=1,ND                   ! GET DESCRIPTORS (IN 16-BIT FORM)
        CALL DESFXY(SEQ_DESCR(I),LF,LX,LY)
        IF (LF.EQ.0) THEN
          IL=IL+1                 ! ONE MORE ELEMENT DESCRIPTOR
          SEQ_DESCR(IL)=LX*1000+LY
        ELSE
          NDL=NDL-1               ! OPERATOR - ONE LESS ELEMENT IN TOTAL
        ENDIF
      ENDDO
      PRINT *,NDL,'ELEMENTS IN SEQUENCE'

! READ IN ELEMENT DESCRIPTORS FROM MERGE TABLE (SKIPPING BLANK LINES
! & ANY ZERO DESCRIPTORS)

      DO I=1,6                    ! SKIP FIRST 6 LINES OF TABLE
        READ (2,'(80X)')
      ENDDO

      I=0
      DO WHILE(IRC.EQ.0)          ! READ DESCRIPTOR & B/G SUBSCRIPT
        READ (2,1,IOSTAT=IRC) LF,LX,LY
    1   FORMAT(I1,I2,I3,55X,I5)
        IF (IRC.EQ.0 .AND. LF.EQ.0 .AND. LX.GT.0 .AND. LY.GT.0) THEN
          I=I+1
          TABLE_DESCR(I)=LX*1000+LY
        ENDIF
      ENDDO
      NM=I
      PRINT *,NM,'ELEMENTS IN TABLE'

! COMPARE THE DESCRIPTORS IN THE SEQUENCE WITH THOSE IN THE TABLE,
! SKIPPING ANY ZEROS IN THE TABLE.

      DO I=1,MIN(NM,NDL)
        IF (TABLE_DESCR(I).EQ.SEQ_DESCR(I)) THEN
          PRINT *,TABLE_DESCR(I),SEQ_DESCR(I)
        ELSE
          PRINT *,TABLE_DESCR(I),SEQ_DESCR(I),'    OUT OF STEP!'
        ENDIF
      ENDDO

      STOP
      END
