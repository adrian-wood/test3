MODULE readlist_mod
  INTERFACE
    SUBROUTINE READLIST (LISTNAME, LTEST, ANAME, IMAP, NAMES, &
                         IFAIL, CERR)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(8),  INTENT(IN)  ::  LISTNAME ! Elements list identifier (see notes)
    LOGICAL,       INTENT(IN)  ::  LTEST ! .TRUE. if diagnostic printout is wanted
    CHARACTER(36), INTENT(OUT) ::  ANAME(*) ! Retrieval element names
    INTEGER,       INTENT(OUT) ::  IMAP(*) ! Map numbers for element names
    INTEGER,       INTENT(OUT) ::  NAMES ! Number of element names read in
    INTEGER,       INTENT(OUT) ::  IFAIL ! Return code
    CHARACTER(*),  INTENT(OUT) ::  CERR ! Error message text

    END SUBROUTINE READLIST
  END INTERFACE
END MODULE readlist_mod
