PROGRAM BUFRENCODE

!-----------------------------------------------------------------------
!
! ROUTINE       : BUFRENCODE
!
! PURPOSE       : Example program to encode a BUFR message.
!
! REVISION INFO :
!
! $Revision: 5$
! $Date: 20/10/2010 10:13:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/BUFRencode.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         20/10/2010 10:13:27    Sheila Needham  Remove
!        print statements
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
! $
! Revision 2.1  2008/09/25
! Bufr message size increased to 250000
!
! Revision 2.0  2001/03/07 10:19:09  usmdb
! Added copyright and modified header/comments - S.Cox
!
! Revision 1.1  1998/10/08 08:52:15  usmdb
! Initial revision
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
USE enbufv4_mod

IMPLICIT NONE

!-----------------------------------------------------------------------
! declare variables.
!-----------------------------------------------------------------------

INTEGER,PARAMETER :: MAXDESC=30            !- max no. of descriptors.
INTEGER,PARAMETER ::   MAXOBS=5                 !- max no. of obs.
INTEGER,PARAMETER ::   RECLEN=25000    !- BUFR message size (record len)

REAL ::   VALUES(MAXOBS*MAXDESC) !- BUFR values array.
INTEGER  ::  DESCR(MAXDESC)         !- BUFR descriptors array.
INTEGER  ::  DATIME(6)              !- date/time array.
INTEGER  ::  NOBS                   !- no. of obs.
INTEGER  ::  NELEM                  !- no. of elements.
INTEGER  ::  NDESC                  !- no. of descriptors.
INTEGER  ::  L                      !- length of BUFR message.
INTEGER  ::  RC                     !- return code from METDB_COPEN
INTEGER  ::  NBYTES                 !- amount of data written to file
INTEGER  ::  IDES                   !- external IDES function.
INTEGER  ::  Edition                !- BUFR edition number
INTEGER  ::  MasterTable            !- BUFR Master table no.
INTEGER  ::  OrigCentre             !- Originating Centre
INTEGER  ::  OrigSubCentre          !- Originating Centre
INTEGER  ::  DataType               !- data category type
INTEGER  ::  LocalDataSubtype       !- data category subtype
INTEGER  ::  IntDataSubtype         !- data category subtype
INTEGER  ::  VerMasTab              !- Version no. of master tables
INTEGER  ::  VerLocTab              !- Version no. of local tables
INTEGER  ::  Sect3Type              !- BUFR section 3 data type
INTEGER  ::  I                      !- used for do loops.
LOGICAL  ::  CMP                    !- BUFR compression flag.
LOGICAL  :: ExtraSect1             !- F for no extra BUFR s.1 data
LOGICAL  :: ExtraSect2             !- FALSE for no BUFR section 2

CHARACTER(LEN=RECLEN) ::  MESAGE       !- final BUFR message.
CHARACTER(LEN=100) ::      NAMES        !- BUFR names string.
CHARACTER(LEN=4) ::         CharSect1    !- extra BUFR sect 1 data
CHARACTER(LEN=1) ::         CharSect2    !- BUFR sect 2 data


!-----------------------------------------------------------------------
! initialise variables.
!-----------------------------------------------------------------------

NDESC=5               !- code 5 descriptors per ob.
NELEM=7               !- code 7 elements per ob.
NOBS=2                !- code 2 observations.
L=0                   !- eventual output from enbufr, octets.
CMP=.TRUE.            !- perform BUFR compression.
MESAGE(:)=' '
NAMES(:)=' '

DO I=1,MAXDESC
  DESCR(I)=0
ENDDO

DO I=1,MAXDESC*MAXOBS
  VALUES(I)=0.0
ENDDO

!-----------------------------------------------------------------------
! code descriptors (5 per ob) - describes 7 elements per ob.
!-----------------------------------------------------------------------

DESCR(1)=IDES(005002)     !- latitude.
DESCR(2)=IDES(006002)     !- longitude.
DESCR(3)=IDES(101004)     !- replication - next descr * 4.
DESCR(4)=IDES(012001)     !- temperature.
DESCR(5)=IDES(001026)     !- storm name (char*8 = 8 bytes)

!-----------------------------------------------------------------------
! code values array.
!-----------------------------------------------------------------------

VALUES(1)=60.0           !- latitude for ob 1.
VALUES(2)=30.0           !- latitude for ob 2.

VALUES(3)=120.0          !- longitude for ob 1.
VALUES(4)=90.0           !- longitude for ob 2.

VALUES(5)=273.0          !- temperature 1 for ob 1.
VALUES(6)=300.0          !- temperature 1 for ob 2.

VALUES(7)=274.0          !- temperature 2 for ob 1.
VALUES(8)=301.0          !- temperature 2 for ob 2.

VALUES(9)=275.0          !- temperature 3 for ob 1.
VALUES(10)=302.0         !- temperature 3 for ob 2.

VALUES(11)=276.0         !- temperature 4 for ob 1.
VALUES(12)=303.0         !- temperature 4 for ob 2.

VALUES(13)=1             !- position in names array for ob 1.
NAMES(1:8)="STORM 01"    !- names array char string for ob 1.

VALUES(14)=9             !- position in names array for ob 2.
NAMES(9:16)="STORM 02"   !- names array char string for ob 2.

!-----------------------------------------------------------------------
! call enbufv4 to code the BUFR message as edition 2.
!-----------------------------------------------------------------------

DATIME(1)    = 2010       !- current year
DATIME(2)    = 10         !- current month
DATIME(3)    = 14         !- current day
DATIME(4)    = 09         !- current hour
DATIME(5)    = 19         !- current minute
DATIME(6)    = 30         !- current second

Edition      = 3          !- use default = 0
MasterTable  = -99        !- use default = 0
OrigCentre   = -99        !- use default (UK Met Office)
OrigSubCentre   = -99     !- use default = 0
DataType     = -99        !- use default = 255
LocalDataSubType  = -99        !- use default = 255
IntDataSubType  = -99        !- use default = 255
VerMasTab    = -99        !- use default = 13
VerLocTab    = -99        !- use default = 0
Sect3Type    = -99        !- use default = 1
ExtraSect1   = .False.   !- no extra BUFR section 1 data
ExtraSect2   = .FALSE.    !- no BUFR section 2 data
CharSect1    = 'TEST'     !- dummy extra BUFR section 1 data
CharSect2    = ' '        !- dummy BUFR section 2 data

print*,'BUFRencode Edition 4.'
print*,'Descr ',DESCR
print*,'Values ',VALUES
print*,'Datime ',DATIME

CALL ENBUFV4(DESCR,VALUES,NDESC,NELEM,NOBS,NAMES,DATIME,    &
     &             MESAGE,CMP,L,Edition,MasterTable,OrigCentre,    &
     &             OrigSubCentre, &
     &             DataType,LocalDataSubType, &
     &             IntDataSubType,VerMasTab,VerLocTab,        &
     &             ExtraSect1,CharSect1,ExtraSect2,CharSect2,Sect3Type)

!-----------------------------------------------------------------------
! Use C I/O to write the BUFR message to the dataset TestMessage.bufr
!-----------------------------------------------------------------------

! Open file.  arg1 = open on unit 20.
!             arg2 = filename = 'TestMessage.bufr'
!             arg3 = filemode = 2 = open for read/write new
!             arg4 = RC = return code. 0 = OK, >0 = failed.

print*,'calling METDB_COPEN'
CALL METDB_COPEN(20,'/u/os/t12sn/port/BUFR_new/TestMessage.bufr',2,RC)

IF (RC /= 0) THEN
  WRITE(6,*)'BUFRencode: ERROR: File Open failed. RC = ',RC
ELSE

! Write file. arg1 = write to unit 20.
!             arg2 = BUFR message to write
!             arg3 = Amount of data written (return value)

  CALL METDB_CWRITE(20,MESAGE(1:L),NBYTES)
  WRITE(6,*)'BUFRencode: INFO: Bytes written to file = ',NBYTES

! Close file. arg1 = close unit 20.

  CALL METDB_CCLOSE(20)
ENDIF

STOP
END
