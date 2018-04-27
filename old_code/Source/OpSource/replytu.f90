SUBROUTINE REPLYTU (REPLY)
!                                                         Arguments
CHARACTER(LEN=4),INTENT(OUT)  ::  REPLY
!                                                   Local variables
INTEGER            :: IOS         ! I/O status code
INTEGER            :: IUNIT       ! HK unit number
INTEGER, PARAMETER :: LENREC=950  ! HK record length
INTEGER            :: RECNO       ! HK record number

CHARACTER(LEN=LENREC) :: REC2     ! HK record 2
CHARACTER(LEN=2)      :: Z0001
CHARACTER(LEN=2)      :: Z0100
!                                              Initialise variables
Z0001=CHAR(0)//CHAR(1)
Z0100=CHAR(1)//CHAR(0)
!                            Read record 2 of housekeeping data set
IUNIT = 1
RECNO = 2
CALL METDB_CREAD_DIR (IUNIT, REC2, LENREC, RECNO, IOS)

!                                  Process flags in bytes 80 and 81
IF (REC2(80:81) == Z0100) THEN
   REPLY = 'U   '                    ! 'Urgent' stop
ELSE IF (REC2(80:81) == Z0001) THEN
   REPLY = 'T   '                    ! 'Tidy' stop
ELSE
   REPLY = '    '                    ! No termination
END IF

RETURN
END SUBROUTINE REPLYTU
