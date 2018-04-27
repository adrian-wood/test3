      SUBROUTINE UADUPS(INDEKS,NTRIES,IDENT,ENTRY,IND)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! ROUTINE       : UADUPS                                              
!                                                                     
! PURPOSE       : To find an index entry for the same ascent as this  
!                 upper air part.                                     
!                                                                     
! DESCRIPTION   : The reason for a separate subroutine is that the    
!                 minute may or may not be set for a particular part  
!                 in the transitional period from launch time only in 
!                 part B to launch time in all parts.  Hence the      
!                 complicated decision process explained below.       
!                    If this part has a launch time (and hence the    
!                 minute is set) & the index entry has a nominal      
!                 hour (minute missing), the index entry time will    
!                 be changed to the launch time - and if the index !1.2
!                 entry has a launch time, this will be set in the !1.2
!                 trailer for this part.                           !1.2
!                 N.B. If the nominal time is the start of an      !1.2
!                   index period and the launch time earlier,      !1.2
!                   different parts of the same ascent will be     !1.2
!                   indexed in different periods and hence never   !1.2
!                   combined.  But with index periods starting at  !1.2
!                   9Z & 21Z this should hardly ever happen.       !1.2
!                                                                     
! CALLED BY     : TAFREP (only for upper air)                         
!                                                                     
! CALLS         : nothing                                             
!                                                                     
! ARGUMENTS     : (1) array of index entries                     (i/o)
!                      (nominal hour may be changed to launch         
!                       time in matching entry on return)             
!                 (2) number of entries in array                  (i) 
!                 (3) identifier to match                         (i) 
!                 (4) entry to match (with hour & minute set)     (i) 
!                 (5) subscript of matching entry (0 if none)     (o) 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:36$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uadups.F,v $
!                                                                     
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:36    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:31  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2001/03/07  11:51:29  11:51:29  usmdb (Generic MetDB account)
! 19 March 2001    C Long
! 1.2  Set launch time in trailer if it's in index from another part.
!      Flag minutes in index if ascent was first indexed under nominal
!      time.
! 1.2a Check lat/long as well as ident when comparing entries in case a
!      mobile part has a wrong figure in a position group.
! 
! Revision 1.1  2000/08/09  14:58:05  14:58:05  usmdb (Generic MetDB account)
! Initial revision
!
! Operational from 17 July 2000   
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

      INTEGER      NTRIES
      CHARACTER*23 INDEKS(NTRIES)
      CHARACTER*23 ENTRY
      CHARACTER*10 IDENT
      INTEGER      IND
      INTEGER      PART_HOUR       ! hour of new part
      INTEGER      PART_MIN        ! minute of new part
      INTEGER      INDEX_HOUR      ! hour of this index entry
      INTEGER      INDEX_MIN       ! minute of this index entry
      INTEGER      I               ! loop variable
      INTEGER      MISSING         ! all ones in one byte          !1.2

      DATA         MISSING/255/

      PART_HOUR=MOD(ICHAR(ENTRY(1:1)),64)
      PART_MIN=ICHAR(ENTRY(2:2))
      IND=0

! Loop round the index entries, stopping if a suitable entry is found.
! (Check lat/long as well as ident in case there's an error in    !1.2a
! the lat/long groups in a mobile part.)                          !1.2a
! If the new part has a time to the minute, then it can be part of an
! ascent for the same hour & minute, obviously, or an ascent with no
! minute set for the same hour or the next.  (The next because e.g.
! British stations & 03953 both launch at, say, 1115Z, but British
! stations set a nominal hour of 11Z, Valentia 12Z.)  A new part   !1.2
! with no minute can be indexed as part of an ascent for the same hour,
! with or without minutes, or the previous hour, with minutes set.
! N.B. The hours compared are relative to the start of the index
! period, currently (July 2000) 9Z or 21Z.

      I=1
      DO WHILE (IND.EQ.0 .AND. I.LE.NTRIES)
        IF (INDEKS(I)(3:11).EQ.IDENT(1:9) .AND.                   !1.2a
     &      INDEKS(I)(13:16).EQ.ENTRY(13:16)) THEN                !1.2a
          INDEX_HOUR=MOD(ICHAR(INDEKS(I)(1:1)),64)
          INDEX_MIN=ICHAR(INDEKS(I)(2:2))
          IF (INDEX_MIN.NE.MISSING) INDEX_MIN=MOD(INDEX_MIN,64)    !1.2
          IF (PART_MIN.EQ.MISSING) THEN                            !1.2
            IF (INDEX_HOUR.EQ.PART_HOUR .OR.
     &         (INDEX_MIN.LT.60 .AND. INDEX_HOUR+1.EQ.PART_HOUR)) THEN
              IND=I
            ENDIF
          ELSE
            IF ((INDEX_HOUR.EQ.PART_HOUR .AND. INDEX_MIN.EQ.PART_MIN)
     &             .OR. (INDEX_MIN.EQ.MISSING .AND.                !1.2
     &                  (INDEX_HOUR.EQ.PART_HOUR .OR.              !1.2
     &                   INDEX_HOUR.EQ.PART_HOUR+1))) THEN         !1.2
              IND=I
                                                                   !1.2
! If launch time is set in this part, set it in index too; flag    !1.2
! minutes to show if launch was in nominal hour or hour before.    !1.2
! (Flags are as follows: top bit set if previous hour, next bit    !1.2
! if same hour; flags only set in index entry, not in trailer.)    !1.2
                                                                   !1.2
              IF (INDEX_MIN.EQ.MISSING .AND. PART_MIN.LT.60) THEN  !1.2
                INDEKS(I)(1:1)=CHAR(PART_HOUR)
                IF (INDEX_HOUR.EQ.PART_HOUR) THEN                  !1.2
                  INDEKS(I)(2:2)=CHAR(64+PART_MIN)                 !1.2
                ELSE                                               !1.2
                  INDEKS(I)(2:2)=CHAR(128+PART_MIN)                !1.2
                ENDIF                                              !1.2
                                                                   !1.2
! If launch time is in index (from another part) but not trailer,  !1.2
! set trailer time to launch time from index.                      !1.2
                                                                   !1.2
              ELSE IF (INDEX_MIN.NE.MISSING .AND.                  !1.2
     &                 PART_MIN.EQ.MISSING) THEN                   !1.2
                ENTRY(1:1)=CHAR(INDEX_HOUR)                        !1.2
                ENTRY(2:2)=INDEKS(I)(2:2)                          !1.2
              ENDIF                                                !1.2
            ENDIF
          ENDIF
        ELSE IF (INDEKS(I)(3:11).EQ.IDENT(1:9) .AND.              !1.2a
     &      INDEKS(I)(13:16).NE.ENTRY(13:16)) THEN                !1.2a
          PRINT *,'UADUPS: same ident & time but lat/long diff:'  !1.2a
          PRINT *,ENTRY,INDEKS(I)                                 !1.2a
        ENDIF
        I=I+1
      ENDDO
      RETURN
      END
