      SUBROUTINE UPAIND(ELMNUM,SEGNUM,SUBNUM,NROWS,STYP,SEGST,
     &                  SEGLEN,NSEGS)                               !2.0

!-----------------------------------------------------------------------
! subroutine     : UPAIND
!
! portablity     : ANSI standard except for '!' used for comments,
!                : IMPLICIT NONE and variable length names greater than
!                : 6 characters.
!
! purpose        : to initialise the index and index structure arrays
!                : for upper air data types.  These arrays must be
!                : consistent with the data dictionary (element numbers)
!                : and with the fixed array of upper-air elements
!                : generated in UPRPARTS
!
! called by      : UPRPARTS
!
! calls          : nothing
!
! Arguments      : ELMNUM   (op) - element nos. as in DDICT
!                : SEGNUM   (op) - segment number
!                : SUBNUM   (op) - subscript no relative to seg start
!                                   as in FinalProfile
!                : NROWS    (op) - no of rows in element table
!                : STYP     (op) - pointer to segment replication count
!                                  0 for mandatory
!                : SEGST    (op) - position of segment start
!                : SEGLEN   (op) - length of segment
!                : NSEGS    (op) - no of segments
!
!Y2K  01.07.1997  UPAIND is Year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/upaind.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:48    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:22  usmdb
! Removed unused dummy arguments QCINC and LFLAG. Added
! copyright and modified header - S.Cox
!
! Revision 1.6  2000/03/10  09:31:53  09:31:53  usmdb (Generic MDB account)
! Implemented 20-03-2000 - S.Cox
! Addition of TOR retrieval elements for each
! part in a sonde ascent.
! 
! Revision 1.5  97/08/04  13:36:43  13:36:43  uspm (Pat McCormack)
! First revisioned version for  1  - with Y2K change
!
! Revision 1.4  1997/07/25 15:20:15  uspm
! Version dated 20-6-97 from  1
!
! Revision 1.3  1997/04/07 13:28:23  uspm
! version dated 8-3-97 from  1
!
! Revision 1.2  1997/02/17 13:32:50  uspm
! Remove entry point as it is a duplicate of one in
! arrindx - sep0196
!
! Revision 1.1  1997/02/17 11:59:07  uspm
! Initial revision
!
! 20-06-97  !B   : S.Cox - Add new elements COLTN_CNTR & BLTN_IDNY
!
! 08-03-97  !A   : S.Cox - Add element PESR_SNSR_FLAG
!
! 01-09-96       : written by S.M.Needham
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

      INTEGER ELMNUM(*)
      INTEGER SEGNUM(*)
      INTEGER SUBNUM(*)
      INTEGER NROWS
      INTEGER STYP(*)
      INTEGER SEGST(*)
      INTEGER SEGLEN(*)
      INTEGER NSEGS

      CHARACTER*132 HEAD

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/upaind.F,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:25:48$ '

! 'RPRT_IDNY                           ',  1
      ELMNUM(1)=1
      SEGNUM(1)=-1
      SUBNUM(1)=10
! 'WMO_BLCK_NMBR                       ',  2
      ELMNUM(2)=2
      SEGNUM(2)=1
      SUBNUM(2)=2
! 'WMO_STTN_NMBR                       ',  3
      ELMNUM(3)=3
      SEGNUM(3)=1
      SUBNUM(3)=4
! 'CALL_SIGN                           ',  4
      ELMNUM(4)=4
      SEGNUM(4)=-2
      SUBNUM(4)=1
! 'LTTD                                ',  5
      ELMNUM(5)=5
      SEGNUM(5)=1
      SUBNUM(5)=8
! 'LNGD                                ',  6
      ELMNUM(6)=6
      SEGNUM(6)=1
      SUBNUM(6)=10
! 'STTN_HGHT                           ',  7
      ELMNUM(7)=7
      SEGNUM(7)=1
      SUBNUM(7)=12
! 'PESR_SNSR_HGHT                      ',  8
      ELMNUM(8)=8
      SEGNUM(8)=1
      SUBNUM(8)=14
! 'YEAR                                ',  9
      ELMNUM(9)=9
      SEGNUM(9)=1
      SUBNUM(9)=16
! 'MNTH                                ', 10
      ELMNUM(10)=10
      SEGNUM(10)=1
      SUBNUM(10)=18
! 'DAY                                 ', 11
      ELMNUM(11)=11
      SEGNUM(11)=1
      SUBNUM(11)=20
! 'HOUR                                ', 12
      ELMNUM(12)=12
      SEGNUM(12)=1
      SUBNUM(12)=22
! 'MINT                                ', 13
      ELMNUM(13)=13
      SEGNUM(13)=1
      SUBNUM(13)=24
! 'RCPT_YEAR                           ', 14
      ELMNUM(14)=14
      SEGNUM(14)=-1
      SUBNUM(14)=1
! 'RCPT_MNTH                           ', 15
      ELMNUM(15)=15
      SEGNUM(15)=-1
      SUBNUM(15)=2
! 'RCPT_DAY                            ', 16
      ELMNUM(16)=16
      SEGNUM(16)=-1
      SUBNUM(16)=3
! 'RCPT_HOUR                           ', 17
      ELMNUM(17)=17
      SEGNUM(17)=-1
      SUBNUM(17)=4
! 'RCPT_MINT                           ', 18
      ELMNUM(18)=18
      SEGNUM(18)=-1
      SUBNUM(18)=5
! 'LEVL_CDTN_CODE                      ', 19
      ELMNUM(19)=19
      SEGNUM(19)=-1
      SUBNUM(19)=11
! 'LEVL_RPLTN_CONT                     ', 20
      ELMNUM(20)=20
      SEGNUM(20)=4
      SUBNUM(20)=2
! 'LEVL_IDNY                           ', 21
      ELMNUM(21)=21
      SEGNUM(21)=5
      SUBNUM(21)=2
! 'LEVL_PESR                           ', 22
      ELMNUM(22)=22
      SEGNUM(22)=5
      SUBNUM(22)=4
! 'LEVL_HGHT                           ', 23
      ELMNUM(23)=23
      SEGNUM(23)=5
      SUBNUM(23)=6
! 'LEVL_TMPR                           ', 24
      ELMNUM(24)=24
      SEGNUM(24)=5
      SUBNUM(24)=8
! 'LEVL_DEW_PONT_TMPR                  ', 25
      ELMNUM(25)=25
      SEGNUM(25)=5
      SUBNUM(25)=10
! 'LEVL_WIND_DRCTN                     ', 26
      ELMNUM(26)=26
      SEGNUM(26)=5
      SUBNUM(26)=12
! 'LEVL_WIND_SPED                      ', 27
      ELMNUM(27)=27
      SEGNUM(27)=5
      SUBNUM(27)=14
! 'MXMM_WIND_LEVL_WIND_SHER_1          ', 28
      ELMNUM(28)=28
      SEGNUM(28)=1
      SUBNUM(28)=26
! 'MXMM_WIND_LEVL_WIND_SHER_2          ', 29
      ELMNUM(29)=29
      SEGNUM(29)=1
      SUBNUM(29)=28
! 'RADI_SNDE_TYPE                      ', 30
      ELMNUM(30)=30
      SEGNUM(30)=1
      SUBNUM(30)=30
! 'TRCKG_SYTM                          ', 31
      ELMNUM(31)=31
      SEGNUM(31)=1
      SUBNUM(31)=32
! 'RADTN_CORTN                         ', 32
      ELMNUM(32)=32
      SEGNUM(32)=1
      SUBNUM(32)=34
! 'SEA_SRFC_TMPR                       ', 33
      ELMNUM(33)=33
      SEGNUM(33)=1
      SUBNUM(33)=36
! 'CLOD_RPLTN_CONT                     ', 34
      ELMNUM(34)=34
      SEGNUM(34)=2
      SUBNUM(34)=2
! 'CLOD_TYPE                           ', 35
      ELMNUM(35)=35
      SEGNUM(35)=3
      SUBNUM(35)=2
! 'CLOD_AMNT                           ', 36
      ELMNUM(36)=36
      SEGNUM(36)=3
      SUBNUM(36)=4
! 'CLOD_BASE_HGHT                      ', 37
      ELMNUM(37)=37
      SEGNUM(37)=3
      SUBNUM(37)=6
! 'PESR_SNSR_FLAG                      ', 38                          !A
      ELMNUM(38)=38                                                   !A
      SEGNUM(38)=-1                                                   !A
      SUBNUM(38)=12                                                   !A
! 'COLTN_CNTR                          ', 39                          !B
      ELMNUM(39)=39                                                   !B
      SEGNUM(39)=-2                                                   !B
      SUBNUM(39)=2                                                    !B
! 'BLTN_IDNY                           ', 40                          !B
      ELMNUM(40)=40                                                   !B
      SEGNUM(40)=-2                                                   !B
      SUBNUM(40)=3                                                    !B
! 'RCPT_YEAR_PARTA                     ', 41                        !1.6
      ELMNUM(41)=134                                                !1.6
      SEGNUM(41)=-1                                                 !1.6
      SUBNUM(41)=13                                                 !1.6
! 'RCPT_MNTH_PARTA                     ', 42                        !1.6
      ELMNUM(42)=135                                                !1.6
      SEGNUM(42)=-1                                                 !1.6
      SUBNUM(42)=14                                                 !1.6
! 'RCPT_DAY_PARTA                      ', 43                        !1.6
      ELMNUM(43)=136                                                !1.6
      SEGNUM(43)=-1                                                 !1.6
      SUBNUM(43)=15                                                 !1.6
! 'RCPT_HOUR_PARTA                     ', 44                        !1.6
      ELMNUM(44)=137                                                !1.6
      SEGNUM(44)=-1                                                 !1.6
      SUBNUM(44)=16                                                 !1.6
! 'RCPT_MINT_PARTA                     ', 45                        !1.6
      ELMNUM(45)=138                                                !1.6
      SEGNUM(45)=-1                                                 !1.6
      SUBNUM(45)=17                                                 !1.6
! 'RCPT_YEAR_PARTB                     ', 46                        !1.6
      ELMNUM(46)=139                                                !1.6
      SEGNUM(46)=-1                                                 !1.6
      SUBNUM(46)=18                                                 !1.6
! 'RCPT_MNTH_PARTB                     ', 47                        !1.6
      ELMNUM(47)=140                                                !1.6
      SEGNUM(47)=-1                                                 !1.6
      SUBNUM(47)=19                                                 !1.6
! 'RCPT_DAY_PARTB                      ', 48                        !1.6
      ELMNUM(48)=141                                                !1.6
      SEGNUM(48)=-1                                                 !1.6
      SUBNUM(48)=20                                                 !1.6
! 'RCPT_HOUR_PARTB                     ', 49                        !1.6
      ELMNUM(49)=142                                                !1.6
      SEGNUM(49)=-1                                                 !1.6
      SUBNUM(49)=21                                                 !1.6
! 'RCPT_MINT_PARTB                     ', 50                        !1.6
      ELMNUM(50)=143                                                !1.6
      SEGNUM(50)=-1                                                 !1.6
      SUBNUM(50)=22                                                 !1.6
! 'RCPT_YEAR_PARTC                     ', 51                        !1.6
      ELMNUM(51)=144                                                !1.6
      SEGNUM(51)=-1                                                 !1.6
      SUBNUM(51)=23                                                 !1.6
! 'RCPT_MNTH_PARTC                     ', 52                        !1.6
      ELMNUM(52)=145                                                !1.6
      SEGNUM(52)=-1                                                 !1.6
      SUBNUM(52)=24                                                 !1.6
! 'RCPT_DAY_PARTC                      ', 53                        !1.6
      ELMNUM(53)=146                                                !1.6
      SEGNUM(53)=-1                                                 !1.6
      SUBNUM(53)=25                                                 !1.6
! 'RCPT_HOUR_PARTC                     ', 54                        !1.6
      ELMNUM(54)=147                                                !1.6
      SEGNUM(54)=-1                                                 !1.6
      SUBNUM(54)=26                                                 !1.6
! 'RCPT_MINT_PARTC                     ', 55                        !1.6
      ELMNUM(55)=148                                                !1.6
      SEGNUM(55)=-1                                                 !1.6
      SUBNUM(55)=27                                                 !1.6
! 'RCPT_YEAR_PARTD                     ', 56                        !1.6
      ELMNUM(56)=149                                                !1.6
      SEGNUM(56)=-1                                                 !1.6
      SUBNUM(56)=28                                                 !1.6
! 'RCPT_MNTH_PARTD                     ', 57                        !1.6
      ELMNUM(57)=150                                                !1.6
      SEGNUM(57)=-1                                                 !1.6
      SUBNUM(57)=29                                                 !1.6
! 'RCPT_DAY_PARTD                      ', 58                        !1.6
      ELMNUM(58)=151                                                !1.6
      SEGNUM(58)=-1                                                 !1.6
      SUBNUM(58)=30                                                 !1.6
! 'RCPT_HOUR_PARTD                     ', 59                        !1.6
      ELMNUM(59)=152                                                !1.6
      SEGNUM(59)=-1                                                 !1.6
      SUBNUM(59)=31                                                 !1.6
! 'RCPT_MINT_PARTD                     ', 60                        !1.6
      ELMNUM(60)=153                                                !1.6
      SEGNUM(60)=-1                                                 !1.6
      SUBNUM(60)=32                                                 !1.6
! 'RPRT_TEXT                           ', 65250
      ELMNUM(61)=65250                                              !1.6
      SEGNUM(61)=-99                                                !1.6
      SUBNUM(61)=0                                                  !1.6

      NROWS=61                                                      !1.6

! segment structure
      NSEGS=5
! non-level elements
      STYP(1)=0
      SEGST(1)=2
      SEGLEN(1)=18
! cloud replication count
      STYP(2)=0
      SEGST(2)=38
      SEGLEN(2)=1
! cloud levels
      STYP(3)=38
      SEGST(3)=40
      SEGLEN(3)=3
! level replication count
      STYP(4)=0
      SEGST(4)=64
      SEGLEN(4)=1
! level elements
      STYP(5)=64
      SEGST(5)=66
      SEGLEN(5)=7

      RETURN
      END
