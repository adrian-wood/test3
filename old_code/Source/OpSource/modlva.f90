      SUBROUTINE MODLVA(IFT,BUFMES,L,IRC)

!-----------------------------------------------------------------------
!
! PROGRAM       : MODLVA
!
! PURPOSE       : to return a bufr message from a model input stream,
!                 a data set with RECFM=FB and messages overflowing
!                 from block to block, so we need 3 blocks in core  !1.8
!                 at a time - assuming no messages are long enough  !1.8
!                 to extend over 4 blocks!                          !1.8
!
! CALLED BY     : MERGE
!
! PARAMETERS    : (1) FT number                               (input)
!                 (2) BUFR message                           (output)
!                 (3) length of BUFR message                 (output)
!                 (4) end of data indicator                  (output)
!                      (0 if may be more messages to come,
!                       -1 if all already returned, >0 if error)
!
!-----------------------------------------------------------------------
! $Log:
!  2    MetDB_Refresh 1.1         02/06/2011 15:07:05    Sheila Needham
!       Replace sequential I/O with direct access
!  1    MetDB_Refresh 1.0         11/04/2011 14:54:22    Sheila Needham
!       Initial F77 versions
! $
! Revision 1.9  2001/02/13 16:27:08  usmdb
! arguments for metdb_copen, metdb_cread changed - S.Cox
!
! Revision 1.8  2001/01/08 14:28:51  usmdb
! 22 Jan 2001    C Long
! 1.8  Keep 3 rather than 2 blocks in core, allowing max message
!      length of 13650
!
! Revision 1.7  2000/08/25  15:15:32  15:15:32  usmdb (Generic MDB account)
! Added T3E preprocessor directive and changed copen,
! cread to metdb_copen, metdb_cread - S.Cox
!
! Revision 1.6  99/12/21  14:09:53  14:09:53  usmdb (Generic MDB account)
! Operational 20-03-2000 - S.Cox
! Correct bug. ift becomes cft.
!
! Revision 1.5  99/05/06  15:00:47  15:00:47  usmdb (Generic MDB account)
! 17-05-1999 - S.Cox
! Correct final read of data. If no data to read - zero the
! 2nd half of the string. Also HP code did not work. Made
! changes so that code works on HP and IBMSP.
!
! Revision 1.4  98/09/17  10:45:48  10:45:48  usmdb (Generic MDB account)
! INQUIRE to avoid error message if no model file.
!
! Revision 1.3  98/02/04  08:41:51  08:41:51  usmdb (Generic MDB account)
! Addition of IBM preprocess directives.
!
! Revision 1.2  1997/10/29 12:48:30  uspm
! Replace FORTRAN open, read by C open, read routines
!
! Revision 1.1  1997/09/23 13:08:17  uspm
! Initial revision
!-----------------------------------------------------------------------

USE inquire_mod

      IMPLICIT NONE                                                 !1.5

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

      INTEGER    LRECL                                              !1.5
      PARAMETER (LRECL=13650)

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER I                                                     !1.5
      INTEGER IFT                                                   !1.5
      INTEGER ILEN                                                  !1.5
      INTEGER IORC                                                  !1.5
      INTEGER IRC                                                   !1.5
      INTEGER IREC                                                  !1.5
      INTEGER L                                                     !1.5
      INTEGER N                                                     !1.5
      INTEGER LFROMCS        ! bytes in BUFR message from CS when   !1.8
                             !  message is split between CS & CS3   !1.8

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL FIRST                                                 !1.5
      LOGICAL THERE          ! true if there is a model file        !1.5

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER           BUFMES*(*)                                !1.5
      CHARACTER*4         BUFR                                      !1.5
      CHARACTER*(LRECL*2) CS          ! string for two              !1.5
                                      ! consecutive blocks          !1.5
      CHARACTER*(LRECL)   CS3         ! string for third block      !1.8
      CHARACTER*8         FTN         ! 'FTnnF001'                  !1.5
      CHARACTER*4         SEVENS                                    !1.5

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

      DATA BUFR/'BUFR'/                                             !1.5
      DATA FIRST/.TRUE./                                            !1.5
      DATA N/1/                                                     !1.5
      DATA SEVENS/'7777'/                                           !1.5

      SAVE
!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

      IRC  = 0                                                      !1.5
      ILEN = 0                                                      !1.5

!-----------------------------------------------------------------------
! First see if there is a model file.  If not, return.
! Read 3 strings of length LRECL the first time, unless end of      !1.8
! data. (IRC>0 means error, IRC<0 end of data)                      !1.5
!-----------------------------------------------------------------------

      IF (FIRST) THEN
        FTN='FTnnF001'                                              !1.5
        WRITE (FTN(3:4),'(I2.2)') IFT                               !1.4
        THERE = INQUIRE (FTN,'DDN')                                 !1.4
        IF (.NOT.THERE) THEN                                        !1.4
          IRC=1                                                     !1.4
          RETURN                                                    !1.4
        ENDIF                                                       !1.4

        OPEN(IFT,FILE='DD:'//FTN,ACTION='READ',   &
     &       ACCESS='DIRECT',RECL=LRECL)

        IREC=1
        READ (IFT,REC=IREC,IOSTAT=IORC) CS(1:LRECL)
        IF (IORC.GT.0) THEN                                         !1.5
          IRC=IORC                                                  !1.5
          RETURN                                                    !1.5
        ELSE IF (IORC.EQ.0) THEN                                    !1.5
          IREC=IREC+1
          READ (IFT,REC=IREC,IOSTAT=IORC) CS(LRECL+1:2*LRECL)
          IF (IORC.EQ.0) THEN                                       !1.8
            IREC=IREC+1
            READ (IFT,REC=IREC,IOSTAT=IORC) CS3(1:LRECL)
          ENDIF                                                     !1.8
        ENDIF                                                       !1.5
        CALL EB2ASC(4,BUFR)
        CALL EB2ASC(4,SEVENS)
        FIRST=.FALSE.
      ENDIF

!-----------------------------------------------------------------------
! Look for start of message, add up section lengths & return message.
!-----------------------------------------------------------------------

      I=INDEX(CS(N:),BUFR)
      IF (I.GT.0) THEN
        N=N+I-1
        IF (ICHAR(CS(N+7:N+7)).GE.2) THEN  !- Bufr edition          !1.5
          L=8+18                                                    !1.5
        ELSE                                                        !1.5
          L=4+18                                                    !1.5
        ENDIF                                                       !1.5
        L=L+INTWO(CS(N+L+1:N+L+2))
        L=L+INTWO(CS(N+L+1:N+L+2))+4

        IF (N+L.LT.2*LRECL) THEN                                    !1.8
          BUFMES=CS(N:N+L)                                          !1.8
        ELSE                                                        !1.8
          LFROMCS=2*LRECL-N+1                                       !1.8
          BUFMES(1:LFROMCS)=CS(N:2*LRECL)                           !1.8
          BUFMES(LFROMCS+1:L)=CS3(1:L-LFROMCS)                      !1.8
        ENDIF                                                       !1.8
        IF (BUFMES(L-3:L).EQ.SEVENS) THEN                           !1.8

!-----------------------------------------------------------------------
! We now have a BUFR message - but reset N for next before returning.
! If the pointer is now in the second record, move the first two    !1.8
! records up LRECL bytes and read a new record into the third.      !1.8
! If the pointer is now in the third record, move the third record  !1.8
! to the first and read new records into the second & third.        !1.8
! 'BUFR' at the start of the next message should then be in CS.     !1.8
!-----------------------------------------------------------------------

          N=N+L
          IF (N.GT.LRECL .AND. N.LE.2*LRECL) THEN                   !1.8
            CS(1:LRECL)=CS(LRECL+1:2*LRECL)
            CS(LRECL+1:2*LRECL)=CS3                                 !1.8

            IF (IORC.EQ.0) THEN
              IREC=IREC+1
              READ (IFT,REC=IREC,IOSTAT=IORC) CS3               !1.8
            END IF
            IF (IORC.NE.0) CS3=' '                                  !1.8

            N=N-LRECL
          ELSE IF (N.GT.2*LRECL) THEN                               !1.8
            CS(1:LRECL)=CS3                                         !1.8

            IF (IORC.EQ.0) THEN
              IREC=IREC+1
              READ(IFT,REC=IREC,IOSTAT=IORC)CS(LRECL+1:2*LRECL) !1.8
            END IF
            IF (IORC.NE.0) CS(LRECL+1:2*LRECL)=' '                  !1.8

            IF (IORC.EQ.0) THEN
              IREC=IREC+1
              READ (IFT,REC=IREC,IOSTAT=IORC) CS3               !1.8
            END IF
            IF (IORC.NE.0) CS3=' '                                  !1.8

            N=N-2*LRECL                                             !1.8
          ENDIF
        ELSE           ! end of data if no 7777 at end
          IRC=-1
        ENDIF
      ELSE
        IRC=-1         ! end of data if no BUFR found.
      ENDIF

      RETURN
      CONTAINS
        FUNCTION INTWO(STR)
        CHARACTER*2 STR
        INTEGER INTWO
        INTWO=ICHAR(STR(1:1))*256+ICHAR(STR(2:2))
        RETURN
        END FUNCTION INTWO
      END
