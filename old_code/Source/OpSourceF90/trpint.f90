SUBROUTINE TRPINT(VALUES,CNAM,YEAR,MONTH,DAY,HOUR,MINT,&
&BULCOD,II,STNID)

!----------------------------------------------------------------------
!
! PROGRAM       : TRPINT
!
! PURPOSE       : TO INITIALISE ELEMENTS OF THE EXPANSION ARRAY WITH
!                 DATA NOT AVAILABLE IN THE REPORT.
!
! DESCRIPTION   : DATA FROM THE INDEX ENTRY AND TRAILER IS USED TO
!                 FILL PART OF THE EXPANSION ARRAY TO ALLOW RETRIEVAL
!                 OF ELEMENTS NOT CONTAINED IN THE REPORT.
!
! DATA TYPE(S)  : Tropical Advisory
!
! CALLED BY     : TFMRET
!
! CALLS         : NONE
!
! PARAMETERS    : (1) VALUES    EXPANSION ARRAY          (I/O)
!                 (2) CNAM      CHARACTER ELEMENT DATA   (O)
!                 (3) LAT       LATITUDE                 (I)
!                 (4) LON       LONGITUDE                (I)
!                 (5) YEAR      YEAR OF REPORT           (I)
!                 (6) MONTH     MONTH OF REPORT          (I)
!                 (7) DAY       DAY OF REPORT            (I)
!                 (8) HOUR      HOUR OF REPORT           (I)
!                 (9) BULCOD    BULLETIN IDENTIFIER      (I)
!                (10) CCCC      COLLECTION CENTRE        (I)
!                (11) STNID     STATION IDENTIFIER       (I)
!
!Y2K  26.06.1997  TRPINT is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trpint.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:59:21  usmdb
! Added copyright and modified header. Removed commented
! out executable line - S.Cox
!
! Revision 1.1  98/06/11  13:40:34  13:40:34  usmdb (Generic MDB account)
! Initial revision
!
! APR 98  INTRODUCED TO ALLOW TROPIACL ADVISORY
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!----------------------------------------------------------------------
! Declare character
!----------------------------------------------------------------------

CHARACTER*(*)     BULCOD     ! Bulletin identifier
CHARACTER*(*)     II         ! II part of TTAAII
CHARACTER*(*)     CNAM       ! String to hold text element data
CHARACTER*(*)     STNID      ! Station identifier

!----------------------------------------------------------------------
! Declare Real
!----------------------------------------------------------------------

REAL              VALUES(*) ! Expanded data values

!----------------------------------------------------------------------
! Declare Integer
!----------------------------------------------------------------------

INTEGER           YEAR       ! Year of observation
INTEGER           MONTH      ! Month of observation
INTEGER           DAY        ! Day of observation
INTEGER           HOUR       ! Hour of observation
INTEGER           MINT

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/trpint.F,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

!----------------------------------------------------------------------
! Initialise the rest of the array values
!----------------------------------------------------------------------

 VALUES(1)=(65536*4)+1       ! TTAAII
 VALUES(2)=YEAR              ! Year of observation
 VALUES(3)=MONTH             ! Month of observation
 VALUES(4)=DAY               ! Day of observation
 VALUES(5)=HOUR              ! Hour of observation
 VALUES(6)=MINT              ! Min of observation
 VALUES(12)=(65536*6)+5      ! Bulletin Id
 VALUES(13)=(65536*4)+11     ! CC
 CNAM(1:4)=STNID
 CNAM(5:10)=BULCOD//II(1:2)
 CNAM(11:14)=STNID

 RETURN
 END SUBROUTINE TRPINT
