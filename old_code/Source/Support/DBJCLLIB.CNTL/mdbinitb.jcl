//M12DBINU JOB (M12,DB,WDS0BF),MDB.TEAM,PRTY=8,MSGCLASS=Q
//*---------------------------------------------------------------------
//*                MCC3.DBJCLLIB.CNTL(MDBINITB)
//*                ----------------------------
//*  TO INITIALISE A MET.D.B. STORAGE DATA SET USING THE NEW FORMAT
//*  DEVELOPED FOR SATELLITE DATA
//*
//* Choose the correct FT01F001 DD statement for MVS or ZFS targets
//*  - For MVS datasets you need to include the DSN, VOLS and number
//*    of CYLS.
//*  - For ZFS datasets you just need the full PATH.
//*
//*  Code NAMELIST Variables as follows:
//*
//*   NAME               CONTENTS
//*   ----               --------
//*   LENBLK    BLOCK SIZE OF STORAGE DATA SET.
//*   NCYLS     DISK SPACE FOR DATA SET (CYLINDERS).
//*   NXVER     INDEX ENTRY FORMAT VERSION NUMBER.
//*   NXBLKS    NUMBER OF INDEX BLOCKS IN DATA SET.
//*   NDXMIN    INDEX PERIOD IN MINUTES.
//*   NDX00Z    INDEX TIME OFFSET FROM 00Z (MINS.)
//*
//* Make sure the LOCALSEQ DD statement refers to the correct
//*   sequence library and member
//*
//*---------------------------------------------------------------------
//INIT EXEC PGM=MDBINITB
//STEPLIB  DD DSN=MPD1.CWPORT.LOADE,DISP=SHR
//* DD statement for an MVS file on a single disk
//* FT01F001 DD DSN=MDB.AIRS,DISP=(,CATLG),
//*             UNIT=DISK,VOL=SER=OPR011,
//*             DCB=(RECFM=F,BLKSIZE=27998),SPACE=(CYL,3)
//* DD statement for a unix file
//* FT01F001 DD PATH='/BPRD/var/mdb/data/ATOVSG/MDB.ATOVSG',
//*            PATHDISP=(KEEP,DELETE),
//*            PATHOPTS=(OCREAT,ORDWR),
//*            PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH)
//FT10F001 DD *
 &STORE_DETAILS  NCYLS=3,LENBLK=27998,NXVER=2,NXBLKS=5,
                 NDXMIN=1440,NDX00Z=0 /
/*
//LOCALSEQ DD DISP=SHR,DSN=SDB.BUFR.LOCALSEQ(AIRS)
//*
//
