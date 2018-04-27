! $Header: analyse.F, 1, 27/02/2007 16:04:56, Stan Kellett$

      SUBROUTINE ANALYSE ( UNIT,FILE,MESGS,SIZE,HEADS,TAILS,NUM_MESGS,
     +                     ERRMSG) 

!  Subroutine to read and analyse a file containing GRIB messages.
!
!  INPUT  :  UNIT      - unit number to connect GRIB file to.
!            FILE      - file name.
!            SIZE      - size of the GRIB file in bytes.
!            ERRMSG    - unit number of file for info/error messages.
!
!  OUTPUT :  HEADS     - array containing number of first octet of
!                        messages.
!            TAILS     - array containing number of last octet of
!                        messages.
!            NUM_MESGS - number of message.
!
! Revision info:
!
! $Revision: 1$ 
! $Date: 27/02/2007 16:04:56$
! $ Source: $
! 
! Change Record:
!
! $Log:
!  1    Met_DB_Project 1.0         27/02/2007 16:04:56    Stan Kellett    
! $
! Revision 6.0  2001/08/09 10:17:09  usmdb
! New version of analyse with preproc. statements replacing .mvs and .unix vers
!
!
!------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER UNIT,SIZE,NUM_MESGS,ERROR
      INTEGER HEADS(*),TAILS(*)
      INTEGER I,ERRMSG

      CHARACTER*50 FILE  

      CHARACTER*1  MESGS(SIZE)
      
! Revision Code
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD = '
     &$Source: /home/us0400/usjh/temp/lib/../../../grib/source/RCS/analyse.F,v $
     &'//'$Date: 27/02/2007 16:04:56$'
        HEADSET = .TRUE.
      ENDIF

!     Set up unit number for error messages.

      IF (ERRMSG.EQ.0) ERRMSG = 6

!     Check that the file should be opened.

      IF (UNIT.NE.0) THEN

!       Uncomment for debug.
!       WRITE (6,*) ' Opening file ',FILE

!       Open file on unit UNIT.
# if defined (MVS) 
        OPEN( UNIT,STATUS='OLD',FORM='UNFORMATTED' )
# else 
        OPEN( UNIT,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED' )
# endif


!       Read in messages to character array.

        READ (UNIT,END=999,IOSTAT=ERROR) (MESGS(I),I=1,SIZE)
 999    CONTINUE

        CLOSE ( UNIT )

      END IF

!     Analyse file using subroutine FIND_MESGS.

      CALL FIND_MESGS (MESGS,SIZE,HEADS,TAILS,NUM_MESGS,ERRMSG)

!     Write out structure.

      WRITE (ERRMSG,*) ' ANALYSE ---------------------------------'
      WRITE (ERRMSG,*)
      WRITE (ERRMSG,*) ' Message No.         Start          End'
      WRITE (ERRMSG,*)
      WRITE (ERRMSG,'(1(5X,I5.5,10X,I9.9,5X,I9.9))') (I,HEADS(I),
     +       TAILS(I),I=1,NUM_MESGS)
      WRITE (ERRMSG,*)
      WRITE (ERRMSG,*) ' ANALYSE ---------------------------------'

      RETURN
      END
