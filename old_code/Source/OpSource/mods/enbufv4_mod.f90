MODULE enbufv4_mod
  INTERFACE
SUBROUTINE ENBUFV4(Descr,Values,Nd,Nelem,Nobs,Names,Datime,         &
                         Mesage,Cmpres,L,Edition,MasterTable,       &
                         VerMasTab,                                 &
                         OrigCentre,OrigSubCentre,                  &
                         DataType,LocalDataSubType,IntDataSubType,  &
                         VerLocTab,ExtraSect1,CharSect1,ExtraSect2, &
                         CharSect2,Sect3Type)
      IMPLICIT NONE
      INTEGER :: NOBS
      INTEGER :: NELEM
      INTEGER :: DESCR(*)
      REAL :: VALUES(NOBS,NELEM)
      INTEGER :: ND
      CHARACTER(LEN=*) :: NAMES
      INTEGER :: DATIME(6)
      CHARACTER(LEN=*) :: MESAGE
      LOGICAL :: CMPRES
      INTEGER :: L
      INTEGER :: EDITION
      INTEGER :: MASTERTABLE
      INTEGER :: ORIGCENTRE
      INTEGER :: ORIGSUBCENTRE
      INTEGER :: DATATYPE
      INTEGER :: LOCALDATASUBTYPE
      INTEGER :: INTDATASUBTYPE
      INTEGER :: VERMASTAB
      INTEGER :: VERLOCTAB
      LOGICAL :: EXTRASECT1
      CHARACTER(LEN=*) :: CHARSECT1
      LOGICAL :: EXTRASECT2
      CHARACTER(LEN=*) :: CHARSECT2
      INTEGER :: SECT3TYPE
    END SUBROUTINE ENBUFV4
  END INTERFACE
END MODULE enbufv4_mod
