      SUBROUTINE SORTR(A,L,N,MASK)

!-----------------------------------------------------------------------
!                                                                     
! ROUTINE       : SORTR                                               
!                                                                     
! PURPOSE       : To sort a 2-dimensional real array on fields        
!               : defined in a mask, numbered in order of importance  
!               : & signed to indicate ascending or descending order. 
!                                                                     
! DESCRIPTION   : This is a fortran version of various sorts based    
!               : on the KDF9 program L82.  It uses a "diminishing    
!               : increment" method based on Frank & Lazarus (Comm    
!               : A.C.M., Jan 1960).  For a general discussion see    
!               : Knuth: "Art of Computer Programming: Sorting and    
!               : Searching" (1973) -  shelf mark EC5b(43).           
!                                                                     
!                 N.B. SORTR is for real numbers, SORTN for integers; 
!                 but SORTN could easily be given the structure of    
!                 SORTR to work with the kind of mask defined here.   
!                 Then mask fields in an existing SORTN call would    
!                 have to be numbered 1,2,... from left to right.     
!                                                                     
! CALLED BY     : UPRPARTS                                            
!                                                                     
! CALLS         : SORTN                                               
!                                                                     
! ARGUMENTS     : (1) array(l,n): n sets of l real numbers            
!               : (2) number of numbers in each item                  
!               : (3) number of items to be sorted                    
!               : (4) mask with zeros for numbers to be ignored       
!                     & nonzero numbers for fields to sort on,        
!                     where magnitude indicates order of importance   
!                     (1 most important, 2 less so, and so on)        
!                     & a negative sign indicates descending order.   
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:23$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sortr.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:23    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:19  usmdb
! Moved declarations of L and N above declarations of arrays of
! size L or N. Added SAVE statement. Added copyright and modified
! header and comments - S.Cox
!
! Revision 1.1  2000/07/10 11:08:11  usmdb
! Initial revision
!
! operational from 17 July 2000 
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER I,J,K,L,M,N,X
      INTEGER MM,NF
      INTEGER MASK(L)
      INTEGER NFIELDS
      INTEGER FIELD(2,10)
      INTEGER FIELD_MASK(2)

      LOGICAL HEADSET

      REAL    A(L,N)
      REAL    R

      CHARACTER HEAD*132

      SAVE                                                          !2.0

      DATA    FIELD_MASK/0,1/
      DATA    HEADSET/.FALSE./

!-----------------------------------------------------------------------
! Revision info
!-----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sortr.F,v $
     &'//'$Date: 30/01/2006 20:24:23$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

!-----------------------------------------------------------------------
! List the fields in order of importance, keeping the position of each
! nonzero mask number & its magnitude, then sorting on the magnitude.
!-----------------------------------------------------------------------

      NFIELDS=0
      DO I=1,L
        IF (MASK(I).NE.0 .AND. NFIELDS.LT.10) THEN
          NFIELDS=NFIELDS+1
          FIELD(1,NFIELDS)=I
          FIELD(2,NFIELDS)=ABS(MASK(I))
        ENDIF
      ENDDO

      CALL SORTN(FIELD,2,NFIELDS,FIELD_MASK)

!-----------------------------------------------------------------------
! First set the increment X.  This will be quartered or halved as the
! sort goes on, being rounded up to an odd number to give a decreasing
! sequence ending with 1.  Items with subscripts X apart will be
! compared and swapped if out of order.
!-----------------------------------------------------------------------

      X=N
   10 X=X/2
      IF (X.GE.8) X=X/2
      IF (MOD(X,2).EQ.0) X=X+1

!-----------------------------------------------------------------------
! Do N minus X comparisons for each increment.  If a swap is done,
! repeat the comparison involving the item just moved up.
!-----------------------------------------------------------------------

      DO K=1,N-X
        I=K
   20   J=I+X

!-----------------------------------------------------------------------
! Where the mask is set, compare the numbers in the two items in turn,
! and if necessary swap them using a single number as work area.
! Loop round the fields in order of importance, comparing the numbers
! in a way that depends on whether the sort for that field is in
! ascending or descending order.  (I.e. read the condition below as
! "if ascending order & I-th greater than J-th or descending order
!  & I-th less than J-th, then swap..."
!-----------------------------------------------------------------------

        DO NF=1,NFIELDS
          M=FIELD(1,NF)
          IF ((MASK(M).GT.0 .AND. A(M,I).GT.A(M,J)) .OR.
     &        (MASK(M).LT.0 .AND. A(M,I).LT.A(M,J))) THEN
            DO MM=1,L
              R=A(MM,I)
              A(MM,I)=A(MM,J)
              A(MM,J)=R
            ENDDO

!-----------------------------------------------------------------------
! If a comparison X back can be repeated, do so. In that way
! each subset of items X apart will be put into order.
!-----------------------------------------------------------------------

            IF (I.GT.X) THEN
              I=I-X
              GO TO 20
            ELSE
              GO TO 50
            ENDIF
          ELSE IF ((MASK(M).GT.0 .AND. A(M,I).LT.A(M,J)) .OR.
     &             (MASK(M).LT.0 .AND. A(M,I).GT.A(M,J))) THEN
            GO TO 50
          ENDIF
        ENDDO
   50   CONTINUE
      ENDDO

!-----------------------------------------------------------------------
! Repeat the process with a smaller increment unless the increment
! has reached 1, in which case the sort has been completed.
!-----------------------------------------------------------------------

      IF (X.GT.1) GO TO 10

      RETURN
      END
