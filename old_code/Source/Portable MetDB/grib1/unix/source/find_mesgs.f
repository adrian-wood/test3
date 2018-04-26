! $Header: find_mesgs.f, 1, 27/02/2007 16:11:06, Stan Kellett$

      SUBROUTINE FIND_MESGS (FILEIN,LEN_FILEIN,STARTS,ENDS,NUM,ERROR)

!     Subroutine to find the first and last octets of all the GRIB
!     messages on a file.
!
!     Input : FILEIN(LEN_FILEIN) character*1 array containing all the
!                                GRIB messages.
!          
!     Output: STARTS integer array containing the number of the first
!                    octet of each message.
!             ENDS   integer array containing the number of the last
!                    octet of each message.
!             NUM    number of messages found.
!

      INTEGER STARTS(*),ENDS(*)
      INTEGER NUM,ERROR

      CHARACTER*1 FILEIN(LEN_FILEIN)

      CHARACTER*4 TEST,TOP,TAIL,MATCH

!     Initialise variables.

      NUM   = 1

!     'GRIB' in ASCII

      TOP   = CHAR(71) // CHAR(82) // CHAR(73) // CHAR(66)

!     '7777' in ASCII

      TAIL  = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55)

      MATCH = TOP

!     Uncomment for debug
!     WRITE (6,'(4(1X,I3.3,1A,1X,Z8))') (II,':',FILEIN(II),II=1,200)

!     Scan through file looking for matches.

      DO I = 1 , LEN_FILEIN - 3

        TEST = FILEIN(I)//FILEIN(I+1)//FILEIN(I+2)//FILEIN(I+3)

!       Uncomment for debug
!       WRITE (6,*) TEST

        IF (TEST.EQ.MATCH) THEN

!         Uncomment for debug
!         WRITE (6,*) ' Found a match.'

!         Found a match.

          IF (MOD(NUM,2).EQ.1) THEN

!           Uncomment for debug.
!           WRITE (6,*) ' Found start of message.'

!           Found the start of a message.

            STARTS(NUM / 2 + 1) = I

            MATCH = TAIL

          END IF

          IF (MOD(NUM,2).EQ.0) THEN

!           Uncomment for debug.   
!           WRITE (6,*) ' Found end of message.'

!           Found the end of a message.

            ENDS(NUM/2) = I + 3

            MATCH = TOP

          END IF

          NUM = NUM + 1
        
        END IF

      END DO

!     NUM should be odd. 

      IF (MOD(NUM,2).EQ.0) THEN

        WRITE (ERROR,*) ' WARNING - incomplete message.'

      ENDIF

!     Return the number of messages.

      NUM = NUM / 2

      RETURN
      END

