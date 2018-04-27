      SUBROUTINE UASORT(ARRAY,QCBIT_ARRAY,VALUES,STANDRD)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : UASORT (upper air storage)                                
!                                                                     
! PURPOSE       : To combine values & q/c bits in one array for
!                 encoding, sorting levels into pressure or height order
!                                                                     
! CALLED BY     : UAEDIT                                                  
!                                                                     
! CALLS         : SORTR                                             !1.4
!                                                                     
! PARAMETERS    : (1) - values                                (i)     
!                 (2) - q/c bits                              (i)     
!                 (3) - merged values & q/c bits              (o)     
!                 (4) - standard/sig flag, true if standard   (i)     
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uasort.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:42    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:36  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  2000/11/07  12:03:11  12:03:11  usmdb (Generic MetDB account)
! 20 Nov 2000    C Long
! Call SORTR rather than SORTN, to sort on kind of level as well as
! pressure or height.  (This should make levels selected by retrieval
! more predictable)
! Complete rewrite to remove redundant arrays and array operations.
! 
! Revision 1.3  98/04/20  07:06:41  07:06:41  usmdb (Generic MDB account)
! Correct subscripts for wind shears
!
! Revision 1.2  97/07/31  11:46:06  11:46:06  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:46:06  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      REAL ARRAY(*)            ! array of decoded values
      REAL QCBIT_ARRAY(*)      ! array of qc bits
      REAL VALUES(*)

      LOGICAL STANDRD          ! true if standard levels

      INTEGER NLEVEL           ! number of levels in profile
      INTEGER NCLOUD           ! number of cloud levels
      INTEGER NLSLOT           ! subscript of NLEVEL in ARRAY
      INTEGER NCSLOT           ! subscript of NCLOUD in ARRAY
      INTEGER NINPUT           ! number of input values
      INTEGER LEVEL1           ! start of first profile level in VALUES
      INTEGER I,J              ! loop variables
      INTEGER PMASK(14)        ! mask to sort on pressure
      INTEGER HMASK(14)        ! mask to sort on height

      CHARACTER*132 HEAD

      DATA HMASK/0,2,3*0,1,8*0/! sort on height & then kind of level
      DATA PMASK/0,2,0,-1,10*0/! sort on pressure & then kind of level
                               !  (descending pressure, hence -1)
      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uasort.F,v $
     &'//'$ $Date: 30/01/2006 20:25:42$ $Revision: 1$'

! To merge q/c bits with values we need to know where the replication
! counts are, for cloud layers (if it's a significant part) or the
! profile.  There are no q/c bits corresponding to these counts (or
! to the 031021 at the start).
!  The numbers below correspond to the current descriptor sequences
! for encoding standard & significant levels, and will have to be
! changed if the sequences change.

      IF (STANDRD) THEN
        NCSLOT=0                    ! no cloud if standard
        NLSLOT=14
      ELSE
        NCSLOT=18
        NCLOUD=ARRAY(NCSLOT)
        NLSLOT=NCSLOT+4*NCLOUD+1    ! 4 values per cloud layer
      ENDIF

      NLEVEL=ARRAY(NLSLOT)
      NINPUT=NLSLOT+7*NLEVEL        ! 7 values per profile level
      IF (STANDRD) NINPUT=NINPUT+2  ! wind shears on end if standard

! Merge the two input arrays, putting q/c bits in front of all values
! except 031021 at start & replication counts.

      J=1                           ! output subscript
      DO I=1,NINPUT
        IF (I.EQ.1 .OR. I.EQ.NCSLOT .OR. I.EQ.NLSLOT) THEN
          VALUES(J)=ARRAY(I)
          J=J+1
          IF (I.EQ.NLSLOT) LEVEL1=J
        ELSE
          VALUES(J)=QCBIT_ARRAY(I)
          VALUES(J+1)=ARRAY(I)
          J=J+2
        ENDIF
      ENDDO

! Sort with pressure mask if first level has pressure, height otherwise.
! This puts surface, tropopause & max wind levels in order and merges
! significant temperatures & winds.
! Levels with the same pressure or height will be ordered by the "kind
! of level" flag (first value for each level).                     !1.4

      IF (VALUES(LEVEL1+3).GT.-9999999.) THEN
        CALL SORTR(VALUES(LEVEL1),14,NLEVEL,PMASK)
      ELSE
        CALL SORTR(VALUES(LEVEL1),14,NLEVEL,HMASK)
      ENDIF
      RETURN
      END
