      LOGICAL FUNCTION OUKSTN(STNNUM)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! Description:
!   This Function returns either .TRUE. if the station number input
!   is a UK overseas station otherwise it returns .FALSE.>
!
! Method:
!   The station number is input to the function as an integer
!   An if statement then checks to see if the station number is one
!   of a list of overseas stations.
!   If it is then the return condition is set to .TRUE.
!   Else the return condition is set to .FALSE.
!   The file for reference purposes which the list of stations was
!   taken from is MDS5.MID**SYF.PICKLIST                            !1.2
!   where ** may change from time to time                           !1.2
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:50$
! $Source: /home/us0400/mdb/op/lib/source/RCS/oukstn.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:50    Sheila Needham  
! $
! Revision 2.1  2002/08/07 09:09:41  usmdb
! 19th Aug 2002 - Kudsia Gwangwaa.
! Added number of ships and bouys to be treated as
! UK stations for decoding sextion 5.
!
! Revision 2.0  2001/07/03  10:43:43  10:43:43  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
! 
! Revision 1.2  2001/04/23  13:58:35  13:58:35  usmdb (Generic MetDB account)
! 23/04/2001 - Updated inline documentation to correctly reflect
! 	       which file has the latest information of Overseas
! 	       UK stations. To meet Standards the initialisation
! 	       of STNLIST was changed to include a DATA statement.
! 	       Removed 10405 from list as now closed.
! 	     				Stan Kellett
! 	
! Revision 1.1  2000/06/09  10:47:54  10:47:54  usmdb (Generic MetDB account)
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
!
! Scalar arguments with INTENT(IN):

      INTEGER STNNUM            ! Station Number input to the function

! Local Parameters:
      INTEGER NUMSTNS           ! number of overseas stations in list
      PARAMETER (NUMSTNS=70)    !2.1 increase to 70 for uk ships

! Local Scalars:
      INTEGER L                 ! loop control Variable

! Local Arrays:
      INTEGER STNLIST(NUMSTNS)  ! List of UK overseas stations
      DATA STNLIST/08495,10401,10320,17601,61901,61902,88883,88878,
     &       88897,88889,62001,62023,62026,62029,62051,62052, !2.1
     &       62081,62101,62103,62105,62106,62107,62109,62112, !2.1
     &       62114,62117,62120,62123,62125,62126,62128,62131, !2.1
     &       62132,62133,62136,62137,62138,62142,62143,62144, !2.1
     &       62145,62147,62152,62155,62162,62163,62166,62168, !2.1
     &       62202,62206,62217,62301,62303,62304,62305,62402, !2.1
     &       62407,62410,62414,63105,63108,63109,63110,63113, !2.1
     &       63116,63117,63119,64045,64046,64049 /            !2.1

! Revision Declarations
      LOGICAL HEADSET           !1.2
      DATA HEADSET /.FALSE./    !1.2
      CHARACTER*132 HEAD

!- End of header

      SAVE HEADSET

! Revision Code as first executable statement
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/oukstn.F,v $
     &'//'$Date: 30/01/2006 20:23:50$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

! Initialise OUKSTN to .FALSE., this is what is returned if
! the station number is not found in the list.
      OUKSTN = .FALSE.

! Initialise Loop control to 1
      L = 1

! Loop over each station until list exhausted or station number
! is found in list of overseas station.
      DO WHILE ((L.LE.NUMSTNS).AND.(STNNUM.NE.STNLIST(L)))
        L=L+1
      ENDDO

! The loop control is now equal to the index of station number
! or is one greater than NUMSTNS.
      IF (L.LE.NUMSTNS) OUKSTN = .TRUE.

! Return and end
      RETURN
      END
