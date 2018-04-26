SUBROUTINE STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST, &
                   IDTYPE,POS,ISTAT,INDREC,INDWMO,NFOUND)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNINDX
!
! PURPOSE       : TO SEARCH FOR STATION IDENTIFIER IN INDEX
!
! DESCRIPTION   : TO RETURN BLOCK NUMBER TO READ FOR STATION DETAILS
!
! DATA TYPE(S)  : STATION NUMBERS, ICAO IDENTIFIERS, DCNN
!
! CALLED BY     : STNRET
!                 STNDISPA
!
! CALLS         : NONE
!
! ARGUMENTS     : (1-4) LIST OF IDENTIFIERS OF ONE TYPE OR ANOTHER
!                 (5) TYPE (1: WMO  2: ICAO  3: DCNN  4: RAINFALL)
!                 (6) IF 'L', RETURN LATEST POSITION ONLY; OTHERWISE
!                      RETURN ALL PAST POSITIONS TOO.
!                 (7) RETURN CODE
!                 (8) RECORD NUMBERS (TO BE RETURNED)
!                 (9) BLOCK/STATION NUMBERS (TO BE RETURNED)
!                 (10) number of record numbers found
!
! REVISION INFO :
!
!
! $Workfile: stnindx.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 19/06/2011 10:40:42$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         19/06/2011 10:40:42    Sheila Needham  Extra
!       argument to return the number of records found
!  4    MetDB_Refresh 1.3         20/12/2010 15:38:34    Sheila Needham
!       Initialise ISTAT
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:10:31    Rosemary Lavery remove
!        old revision info
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments

INTEGER,           INTENT(IN)  :: WMOLST(49)
CHARACTER (LEN=4), INTENT(IN)  :: ICAOLST(49)
INTEGER,           INTENT(IN)  :: DCNNLST(49)
INTEGER,           INTENT(IN)  :: RAINLST(49)
INTEGER,           INTENT(IN)  :: IDTYPE
CHARACTER (LEN=1), INTENT(IN)  :: POS
INTEGER,           INTENT(OUT) :: ISTAT
INTEGER,           INTENT(OUT) :: INDREC(1000)
INTEGER,           INTENT(OUT) :: INDWMO(1000)
INTEGER,           INTENT(OUT) :: NFOUND

! Local Variables

CHARACTER (LEN=4)              :: ICAOSTN(8000)
CHARACTER (LEN=4)              :: ICAOID
CHARACTER (LEN=4)              :: KEPICAO

INTEGER                        :: WMOID
INTEGER                        :: DCNNID
INTEGER                        :: RAINID
INTEGER                        :: COUNT
INTEGER                        :: WMOBLK(0:99)
INTEGER                        :: NWMOBLK(0:99)
INTEGER                        :: WMOSTN(15000)
INTEGER                        :: WMOREC(15000)
INTEGER                        :: DCNNSTN(500)
INTEGER                        :: DCNNREC(500)
INTEGER                        :: RAINSTN(500)
INTEGER                        :: RAINREC(500)
INTEGER                        :: ICAOREC(8000)
INTEGER                        :: LWMO = 0        ! TEMPORARY COUNTS
INTEGER                        :: LICAO = 0
INTEGER                        :: LDCNN = 0
INTEGER                        :: LRAIN = 0
INTEGER                        :: NWMO = 0        ! MAIN COUNTS
INTEGER                        :: NICAO = 0
INTEGER                        :: NDCNN = 0
INTEGER                        :: NRAIN = 0
INTEGER                        :: I,J,K           ! loop counters
INTEGER                        :: N
INTEGER                        :: I0
INTEGER                        :: KEPSTN
INTEGER                        :: KEPRAIN
INTEGER                        :: KEPDCNN
INTEGER                        :: IBLOCK
INTEGER                        :: IREC

LOGICAL                        :: REGION


COMMON /STNRECNO/  WMOSTN,WMOREC,  DCNNSTN,DCNNREC, &
                  RAINSTN,RAINREC, ICAOSTN,ICAOREC

DATA WMOBLK/100*0/
DATA NWMOBLK/100*0/

! INITIALISE VARIABLES


REGION=.FALSE.
NFOUND=0
COUNT=1

!**********************************************************************
! TAKE NEXT IDENTIFIER FROM LIST
!**********************************************************************

100 CONTINUE

IF_IDTYPEA: &
IF (IDTYPE == 1) THEN
  IF (WMOLST(COUNT) /= 0) THEN
    WMOID=WMOLST(COUNT)
    COUNT=COUNT+1
  ELSE
    GO TO 990
  END IF

ELSE IF (IDTYPE == 2) THEN
  IF (ICAOLST(COUNT) /= '    ') THEN
    ICAOID=ICAOLST(COUNT)(1:4)
    COUNT=COUNT+1
  ELSE
    GO TO 990
  END IF

ELSE IF (IDTYPE == 3) THEN
  IF (DCNNLST(COUNT) /= 0) THEN
    DCNNID=DCNNLST(COUNT)
    COUNT=COUNT+1
  ELSE
    GO TO 990
  END IF

ELSE IF (IDTYPE == 4) THEN
  IF (RAINLST(COUNT) /= 0) THEN
    RAINID=RAINLST(COUNT)
    COUNT=COUNT+1
  ELSE
    GO TO 990
  END IF
END IF IF_IDTYPEA

!**********************************************************************
!
! FIRST LOOK FOR IDENTIFIER IN TEMPORARY RECORDS
!
! (READ RECORD ONLY IF FIRST TIME FOR THIS KIND OF IDENTIFIER)
!
!**********************************************************************

IF_IDTYPEB: &
IF (IDTYPE == 1) THEN
  IF (NWMO == 0) THEN
    READ (89,'(5X,I2,1X,50(I5,1X,I5,1X))',REC=590) LWMO, &
     (WMOSTN(I),WMOREC(I),I=1,LWMO)
  END IF

  DO I=1,LWMO
    IF (WMOID == WMOSTN(I)) THEN
      IF (POS == 'L') THEN
        NFOUND=NFOUND+1
        INDREC(NFOUND)=WMOREC(I)
        GO TO 900
      END IF
    END IF
  END DO


ELSE IF (IDTYPE == 2) THEN
  IF (NICAO == 0) THEN
    READ (89,'(5X,I2,1X,50(A4,1X,I5,1X))',REC=591) LICAO, &
     (ICAOSTN(I),ICAOREC(I),I=1,LICAO)
  END IF

  DO I=1,LICAO
    IF (ICAOID == ICAOSTN(I)) THEN
      IF (POS == 'L') THEN
        NFOUND=NFOUND+1
        INDREC(NFOUND)=ICAOREC(I)
        GO TO 900
      END IF
    END IF
  END DO


ELSE IF (IDTYPE == 3) THEN
  READ (89,'(5X,I2,1X,50(I4,1X,I5,1X))',REC=592) LDCNN, &
   (DCNNSTN(I),DCNNREC(I),I=1,LDCNN)

  DO I=1,LDCNN
    IF (DCNNID == DCNNSTN(I)) THEN
      NFOUND=NFOUND+1
      INDREC(NFOUND)=DCNNREC(I)
      GO TO 900
    END IF
  END DO


ELSE IF (IDTYPE == 4) THEN
  READ (89,'(5X,I2,1X,50(I6,1X,I5,1X))',REC=593) LRAIN, &
   (RAINSTN(I),RAINREC(I),I=1,LRAIN)

  DO I=1,LRAIN
    IF (RAINID == RAINSTN(I)) THEN
      NFOUND=NFOUND+1
      INDREC(NFOUND)=RAINREC(I)
      GO TO 900
    END IF
  END DO
END IF IF_IDTYPEB

!**********************************************************************
!
! IF IDENTIFIER NOT FOUND IN TEMPORARY RECORDS, LOOK IN MAIN INDEX
!
!**********************************************************************
!
! IF WMO BLK AND STN
!
!**********************************************************************

IF_IDTYPEC: &
IF (IDTYPE == 1) THEN
  IF (WMOID <= 99) REGION=.TRUE.

! WHERE ENTRIES FOR WMO BLOCK N START, NWMOBLK(N) WILL BE THE NUMBER
! OF STATIONS IN BLOCK N.

  IF (WMOBLK(1) == 0) THEN
    READ (89,'(5X,50(I2,1X,I5,1X))',REC=1) (N,WMOBLK(N),I=1,50)
    READ (89,'(5X,50(I2,1X,I5,1X))',REC=2) (N,WMOBLK(N),I=1,50)
  END IF

! GET WMO BLOCK NUMBER.  INDEXES FOR WMO BLOCKS WILL BE READ AS NEEDED.

  IF (REGION) THEN
    IBLOCK=WMOID
  ELSE
    IBLOCK=WMOID/1000
  END IF
  IF (IBLOCK < 1 .OR. IBLOCK > 99) GOTO 900
  IF (WMOBLK(IBLOCK) == 0) GOTO 900


  IF (NWMOBLK(IBLOCK) == 0) THEN
    IREC=WMOBLK(IBLOCK)

210 CONTINUE
    J=LWMO+(IREC-WMOBLK(1))*50
    READ(89,'(5X,I2,1X,50(I5,1X,I5,1X))',REC=IREC) &
      N,(WMOSTN(I),WMOREC(I),I=J+1,J+N)

    NWMOBLK(IBLOCK)=NWMOBLK(IBLOCK)+N
    IF (N == 50) THEN
      IREC=IREC+1
      GO TO 210
    END IF
    NWMO=NWMO+NWMOBLK(IBLOCK)
  END IF

! FIND STATION IN INDEX ENTRIES FOR ITS WMO BLOCK.

IF_WMOBLK: &
  IF (.NOT.REGION) THEN
    I0=LWMO+(WMOBLK(IBLOCK)-WMOBLK(1))*50
    DO I=I0+1,I0+NWMOBLK(IBLOCK)
      IF (WMOID == WMOSTN(I)) THEN
        IF (POS == 'L') THEN
          KEPSTN=WMOSTN(I)
          DO K=1,10
            IF (KEPSTN /= WMOSTN(I+K)) THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=WMOREC(I+K-1)
              INDWMO(NFOUND)=WMOSTN(I+K-1)
              GOTO 900
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=WMOREC(I)
          INDWMO(NFOUND)=WMOSTN(I)
        END IF
      END IF
    END DO


  ELSE IF (REGION) THEN
    I0=LWMO+(WMOBLK(IBLOCK)-WMOBLK(1))*50

DO_WMOBLK: &
    DO I=I0+1,I0+NWMOBLK(IBLOCK)
      IF (WMOID == WMOSTN(I)/1000) THEN
        IF (POS == 'L') THEN
          IF (KEPSTN == WMOSTN(I)) GOTO 230
          KEPSTN=WMOSTN(I)
          DO K=1,10
            IF (KEPSTN /= WMOSTN(I+K)) THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=WMOREC(I+K-1)
              INDWMO(NFOUND)=WMOSTN(I+K-1)
              GOTO 230
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=WMOREC(I)
          INDWMO(NFOUND)=WMOSTN(I)
        END IF
      END IF
230   CONTINUE
    END DO DO_WMOBLK
  END IF IF_WMOBLK

!**********************************************************************
!    IF ICAOID
!**********************************************************************

ELSE IF (IDTYPE == 2) THEN
  IF (ICAOID(3:3) == ' ') REGION=.TRUE.


  IF (NICAO == 0) THEN
    IREC=350                   ! START READING AT BLOCK 350

310 CONTINUE
    J=LICAO+(IREC-350)*50
    READ(89,'(5X,I2,1X,50(A4,1X,I5,1X))',REC=IREC) &
      N,(ICAOSTN(I),ICAOREC(I),I=J+1,J+N)
    NICAO=NICAO+N
    IF (N == 50) THEN
      IREC=IREC+1
      GO TO 310
    END IF
  END IF


IF_ICAOBLK: &
  IF (.NOT.REGION) THEN
    IF (ICAOID < 'AAAA' .OR. ICAOID > 'ZZZZ') GOTO 900

DO_ICAOSITE: &
    DO I=LICAO+1,LICAO+NICAO
      IF (ICAOID == ICAOSTN(I)) THEN
        IF (POS == 'L') THEN
          KEPICAO=ICAOSTN(I)
          DO K=1,10
            IF (KEPICAO /= ICAOSTN(I+K)) THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=ICAOREC(I+K-1)
              GOTO 900
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=ICAOREC(I)
        END IF
      END IF
    END DO DO_ICAOSITE


  ELSE IF (REGION) THEN
    IF (ICAOID(1:2) < 'AA' .OR. ICAOID(1:2) > 'ZZ') GOTO 900

DO_ICAOBLK: &
    DO I=LICAO+1,LICAO+NICAO
      IF (ICAOID(1:2) == ICAOSTN(I)(1:2)) THEN
        IF (POS == 'L') THEN
          IF (KEPICAO == ICAOSTN(I)) GOTO 330
          KEPICAO=ICAOSTN(I)
          DO K=1,10
            IF(KEPICAO /= ICAOSTN(I+K))THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=ICAOREC(I)
              GOTO 330
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=ICAOREC(I)
        END IF
      END IF
330   CONTINUE
    END DO DO_ICAOBLK
  END IF IF_ICAOBLK

!**********************************************************************
!    IF DCNNID
!**********************************************************************

ELSE IF (IDTYPE == 3) THEN
  IF (DCNNID <= 99) REGION=.TRUE.


  IF (NDCNN == 0) THEN
    IREC=500                   ! START READING AT BLOCK 500

410 CONTINUE
    J=LDCNN+(IREC-500)*50
    READ(89,'(5X,I2,1X,50(I4,1X,I5,1X))',REC=IREC) &
      N,(DCNNSTN(I),DCNNREC(I),I=J+1,J+N)

    NDCNN=NDCNN+N
    IF (N == 50) THEN
      IREC=IREC+1
      GO TO 410
    END IF
  END IF

IF_DCNBLK: &
  IF (.NOT.REGION) THEN
    IF (DCNNID < 0 .OR. DCNNID > 9999) GOTO 900

DO_DCNSITE: &
     DO I=LDCNN+1,LDCNN+NDCNN
       IF (DCNNID == DCNNSTN(I)) THEN
         IF (POS == 'L') THEN
           KEPDCNN=DCNNSTN(I)
           DO K=1,10
             IF(KEPDCNN /= DCNNSTN(I+K))THEN
               NFOUND=NFOUND+1
               INDREC(NFOUND)=DCNNREC(I+K-1)
               GOTO 900
             END IF
           END DO
         ELSE
           NFOUND=NFOUND+1
           INDREC(NFOUND)=DCNNREC(I)
         END IF
       END IF
     END DO DO_DCNSITE

  ELSE IF (REGION) THEN
    IF (DCNNID < 0 .OR. DCNNID > 99) GOTO 900

     DO_DCNBLK: &
     DO I=LDCNN+1,LDCNN+NDCNN
       IF (DCNNID == DCNNSTN(I)/100) THEN
         IF (POS == 'L') THEN
           KEPDCNN=DCNNSTN(I)
           DO K=1,10
             IF(KEPDCNN /= DCNNSTN(I+K))THEN
               NFOUND=NFOUND+1
               INDREC(NFOUND)=DCNNREC(I+K-1)
               GOTO 430
             END IF
           END DO
         ELSE
           NFOUND=NFOUND+1
           INDREC(NFOUND)=DCNNREC(I)
         END IF
       END IF
430    CONTINUE
     END DO DO_DCNBLK
  END IF IF_DCNBLK

!**********************************************************************
! IF RAINID
!**********************************************************************

ELSE IF (IDTYPE == 4) THEN
  IF (RAINID <= 99) REGION=.TRUE.


  IF (NRAIN == 0) THEN
    IREC=550                   ! START READING AT BLOCK 550

510 CONTINUE
    J=LRAIN+(IREC-550)*50
    READ(89,'(5X,I2,1X,50(I6,1X,I5,1X))',REC=IREC) &
      N,(RAINSTN(I),RAINREC(I),I=J+1,J+N)

    NRAIN=NRAIN+N
    IF (N == 50) THEN
      IREC=IREC+1
      GO TO 510
    END IF
  END IF

IF_RAINBLK: &
  IF (.NOT.REGION) THEN
    IF (RAINID < 0 .OR. RAINID > 999999) GOTO 900

    DO_RAINSITE: &
    DO I=LRAIN+1,LRAIN+NRAIN
      IF (RAINID == RAINSTN(I)) THEN
        IF (POS == 'L') THEN
          KEPRAIN=RAINSTN(I)
          DO K=1,10
            IF (KEPRAIN /= RAINSTN(I+K)) THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=RAINREC(I+K-1)
              GOTO 900
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=RAINREC(I)
        END IF
      END IF
    END DO DO_RAINSITE

  ELSE IF (REGION) THEN
    IF (RAINID < 0 .OR. RAINID > 99) GOTO 900

    DO_RAINBLK: &
    DO I=LRAIN+1,LRAIN+NRAIN
      IF (RAINID == RAINSTN(I)/10000) THEN
        IF (POS == 'L') THEN
          IF (KEPRAIN == RAINSTN(I)) GOTO 530
          KEPRAIN=RAINSTN(I)
          DO K=1,10
            IF (KEPRAIN /= RAINSTN(I+K)) THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=RAINREC(I+K-1)
              GOTO 530
            END IF
          END DO
        ELSE
          NFOUND=NFOUND+1
          INDREC(NFOUND)=RAINREC(I)
        END IF
      END IF
530   CONTINUE
    END DO DO_RAINBLK
  END IF IF_RAINBLK
END IF IF_IDTYPEC

!**********************************************************************
!
! SEE IF THERE'S ANOTHER IDENTIFIER.  IF NOT, SET RETURN CODE
! & PUT ZERO IN ARRAY OF RECORD NUMBERS AFTER LAST NUMBER FOUND.
!
!**********************************************************************

900  IF (COUNT < 50) GO TO 100

990  CONTINUE
ISTAT = 0
IF (NFOUND == 0) THEN
  ISTAT=8
ELSE IF (NFOUND == 1) THEN
  ISTAT=0
ELSE IF (NFOUND > 1) THEN
  ISTAT=4
END IF
INDREC(NFOUND+1)=0


RETURN
END SUBROUTINE STNINDX
