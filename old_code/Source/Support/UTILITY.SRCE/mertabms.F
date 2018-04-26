      PROGRAM MERTABMS

!-----------------------------------------------------------------------
! TO CHECK DESCRIPTORS IN MODEL FILE AGAINST MERGE TABLE
! (input line says which column the subscripts are in:
! A - ANALYSIS, B - BACKGROUND, M - MODEL LEVELS)
!-----------------------------------------------------------------------

      INTEGER MODEL_DESCR(999),MERGE_DESCR(999),LMODEL(4,999),BSQ
      LOGICAL CALL_STRING
      CHARACTER MESS*25000,STR*2,INPUT*2,MERLINE*80
      CHARACTER*17 FORM(4)/'(I1,I2,I3,50X,I5)','(I1,I2,I3,55X,I5)',
     &                     '(I1,I2,I3,60X,I5)','(I1,I2,I3,65X,I5)'/

      INTWO(STR)=ICHAR(STR(1:1))*256+ICHAR(STR(2:2))

!-----------------------------------------------------------------------
! read kind of model file (analysis or background, reported or model
! levels) to see which column to check (which format to read with).
!-----------------------------------------------------------------------

      READ (*,'(A2)') INPUT
      IF (INPUT(1:1).EQ.'A' .OR. INPUT(2:2).EQ.'A') INFORM=1
      IF (INPUT(1:1).EQ.'B' .OR. INPUT(2:2).EQ.'B') INFORM=2
      IF (INPUT(1:1).EQ.'M' .OR. INPUT(2:2).EQ.'M') INFORM=INFORM+2

!-----------------------------------------------------------------------
! READ FIRST DATA MESSAGE FROM MODEL FILE (SKIPPING REQUEST(S))
!-----------------------------------------------------------------------

      CALL_STRING = .TRUE.
      DO WHILE (CALL_STRING)
        CALL MODELB(1,MESS,L,IRC)   ! REQUEST
        BSQ = 0
        IF (ICHAR(MESS(8:8)).GT.1) BSQ=4
        IDESC=ICHAR(MESS(30+BSQ:30+BSQ))*256+ICHAR(MESS(31+BSQ:31+BSQ))
        CALL DESFXY(IDESC,LF,LX,LY)
        CALL_STRING = (LF.EQ.2 .AND. LX.EQ.5)
      ENDDO

      L3=INTWO(MESS(24:25))       ! LENGTH OF SECTION WITH DESCRIPTORS
      ND=(L3-7)/2                 ! NUMBER OF DESCRIPTORS

!-----------------------------------------------------------------------
! put message descriptors in fxxyyy form, omitting operators & 031001
!-----------------------------------------------------------------------

      PRINT *,'DESCRIPTORS IN ASSOC DATA MESSAGE'
      NDL=ND                      ! NUMBER OF ELEMENT DESCRIPTORS
      IL=0
      J=1                         ! SUBSCRIPT IN MODEL_DESCR ARRAY
      DO I=1,ND                   ! GET DESCRIPTORS (IN 16-BIT FORM)
        MODEL_DESCR(J)=INTWO(MESS(28+I*2:29+I*2))
        CALL DESFXY(MODEL_DESCR(J),LF,LX,LY)
        IF (LF.EQ.0 .AND. .NOT.(LX.EQ.31.AND.LY.EQ.1)) THEN
          IL=IL+1                 ! ONE MORE ELEMENT DESCRIPTOR
          MODEL_DESCR(IL)=LX*1000+LY
          PRINT *,MODEL_DESCR(IL)
        ELSE
          NDL=NDL-1               ! OPERATOR - ONE LESS ELEMENT IN TOTAL
        ENDIF
        IF (LF.EQ.0 .AND. I.LT.ND) J=J+1
      ENDDO
      NM=J

!-----------------------------------------------------------------------
! Get descriptors and pointers to model inputs from merge table.
!-----------------------------------------------------------------------

      I=1
      DO WHILE (IRC.EQ.0)
        READ (2,IOSTAT=IRC) MERLINE
        IF (MERLINE(1:1).EQ.'0') THEN
          READ (MERLINE,1) LX,LY,
     &      LMODEL(1,I),LMODEL(2,I),LMODEL(3,I),LMODEL(4,I)
!             analysis, background, model levels analysis & background
    1     FORMAT (2I3,50X,4I5)
          MERGE_DESCR(I)=LX*1000+LY
          I=I+1
        ENDIF
      ENDDO
      NT=I

!-----------------------------------------------------------------------
! Compare the descriptors in the model input with those in the table,
! printing the n-th element descriptor from the message if n is the
! appropriate subscript in the table.
!-----------------------------------------------------------------------

      PRINT *,'CORRESPONDENCE BETWEEN TABLE & MESSAGE DESCRIPTORS'
      DO I=1,NT
        IF (LMODEL(INFORM,I).NE.0) THEN
          PRINT *,MERGE_DESCR(I),MODEL_DESCR(LMODEL(INFORM,I))
        ENDIF
      ENDDO

      STOP
      END
