      SUBROUTINE EERROR(REP,IRC)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : EERROR                                              
!                                                                     
! PURPOSE       : TO DETECT TRANSMISSION ERRORS MARKED BY E'S AND     
!                 REMOVE THE BAD GROUP IF A 5-FIGURE GROUP FOLLOWS    
!                                                                     
! DESCRIPTION   : TC CODE REMOVES ERRORS MARKED BY 'E E E'; THIS      
!                 PROGRAM COPES WITH OTHER COMBINATIONS OF E & SPACE  
!                                                                     
! CALLED BY     : SYNBUL                                              
!                                                                     
! CALLS         : IVALUE                                              
!                                                                     
! PARAMETERS    : (1) REPORT                                          
!                 (2) RETURN CODE, 4 IF BAD GROUP REMOVED, 0 OTHERWISE
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:11$
! $Source: /home/us0400/mdb/op/lib/source/RCS/eerror.F,v $
!
! CHANGE RECORD :
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:11    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:43  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.4  99/04/12  10:48:04  10:48:04  usmdb (Generic MetDB account)
! 19-04-1999 S.Cox - ref MetDB problems 438,440
! To prevent out of bounds error
! 
! Revision 1.3  97/08/07  09:54:06  09:54:06  uspm (Pat McCormack)
! Change check for figure on character before space so will
! work on EBCDIC and ASCII machines
!
! Revision 1.2  1997/07/31 09:26:02  uspm
! First revision for MVS
!
! Revision 1.1  1997/07/04 12:37:15  uspm
! Initial revision
!
! AUG 96: COMPARE START & END BEFORE SETTING STRING TO SPACES        !A
!         (END LESS THAN START LEADS TO DRASTIC OVERWRITING!)        
!                                                                     
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      CHARACTER REP*(*), CH*1
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/eerror.F,v $
     &'//'$ $Date: 30/01/2006 20:22:11$ $Revision: 1$'

      IRC=0
*
      IEE=INDEX(REP,'E')            ! LOOK FOR E
      IF (IEE.EQ.0) RETURN          ! IF NONE, RETURN
      IE=IEE                        ! IF E FOUND, KEEP DISPLACEMENT
   10 IF (IEE.GE.LEN(REP)) RETURN   ! STOP IF END OF REPORT REACHED
      ISP=INDEX(REP(IEE:),' ')      ! LOOK FOR NEXT SPACE AFTER E
      IF (ISP.EQ.0) RETURN          ! IF NONE, END OF REPORT - RETURN
*
      IEE=IEE+ISP                   ! POINT PAST THIS SPACE
      CH=REP(IEE-2:IEE-2)           ! LOOK AT CHARACTER BEFORE SPACE
      IF (CH.EQ.'E' .OR. CH.EQ.' ') THEN   ! 'E' OR ANOTHER SPACE?   !A
        GO TO 10                    ! IF SO, LOOK ON FOR NEXT SPACE
      ELSE IF (CH.GE.'0' .AND.      ! IF NOT, IS IT A FIGURE?
     &         CH.LE.'9') THEN                                    ! v1.3
        IGROUP=IVALUE(REP(IEE-6:IEE-2))  ! IF SO, DO FIGURES FOLLOW?
        IF (IGROUP.LT.0) RETURN     ! IF NOT 5 FIGURES, GIVE UP.
      ELSE                                                           !A
        RETURN                                                       !A
      ENDIF
*
* WE'VE FOUND A 5-FIGURE GROUP AFTER THE E'S.  NOW LOOK BEFORE THE
* FIRST 'E' TO SEE WHAT TO REMOVE.
*
      NFIGS=0                       ! COUNT FIGS TO SEE IF ANY FOUND
      IBACK=1                       ! START BEFORE FIRST E
      IF (IE.LE.1) RETURN           ! GIVE UP                       !1.4
   20 CH=REP(IE-IBACK:IE-IBACK)     ! PREVIOUS CHARACTER
      IF (CH.EQ.' ' .AND. NFIGS.EQ.0) THEN  ! SPACE BEFORE E'S?
        IBACK=IBACK+1               ! BACK ONE MORE TO FIND BAD GROUP
        GO TO 20
      ELSE IF (CH.GE.'0' .AND. CH.LE.'9') THEN
        NFIGS=NFIGS+1               ! FIGURE IN BAD GROUP
        IBACK=IBACK+1               ! BACK ONE MORE TO FIND START
        GO TO 20
      ELSE IF (CH.LE.' '.AND.NFIGS.GT.0.AND.IE-IBACK.LT.IEE-7) THEN  !A
        REP(IE-IBACK:IEE-7)=' '     ! SET BAD GROUP & E'S TO SPACES
        IRC=4
        WRITE(6,*)'EERROR: CORRECTED REPORT = ',REP                 !1.4
      ENDIF
      RETURN                        ! OR CAN WE LOOK FOR MORE E'S?
      END
