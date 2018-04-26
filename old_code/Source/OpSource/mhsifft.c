/*====================================================================*/
/* MHSIFFC: pure c code     | Main use is as an alternative for the   */
/* LIBRARY:                 | assembly written code MHSIFF:           */
/* CREATED: April 2011      |                                         */
/* VERSION: 1.0             | Deletes, renames and scans for MHS      */
/* WRITTEN: P Nelson        | datasets.                               */
/* MODIFIED: S Needham      | For integration test capability.        */
/* SECTION: TIS             |                                         */
/*====================================================================*/
/*                                                                    */
/*    This C sample program shows how to use the z/OS Catalog Search  */
/*    Interface (CSI) to get dataset information. This utility is     */
/*    very handy for searcing through catalogs for datasets.          */
/*                                                                    */
/*    The CSI can be used to search through catalogs, and provide     */
/*    any information required. Searches can be done by dataset name  */
/*    dataset pattern, dataset type, and catalog name.                */
/*                                                                    */
/*    Any of the information in the catalog can be output by          */
/*    specifying the field names required in the filter key section.  */
/*                                                                    */
/*    In this example, we're specifying a fixed dataset pattern,      */
/*    getting all types of datasets from all catalogs, and getting    */
/*    one extra field: volume serial number. IGGCSI00 can also get    */
/*    ATL information. But we're not doing that here.                 */
/*                                                                    */
/* This program is free software: you can redistribute it and/or      */
/* modify it under the terms of the GNU General Public License as     */
/* published by the Free Software Foundation, either version 3 of the */
/* License, or (at your option) any later version.                    */
/*                                                                    */
/* ================================================================== */
/* Includes                                                           */
/* ================================================================== */
/* --- Includes  ---------------------------------------------------- */
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
/* agma runopts(POSIX(ON)) */

/* ================================================================== */
/* Structures to Map IGGCSI00 input and output                        */
/* ================================================================== */
/* --- Filter Parameter Structure ----------------------------------- */
typedef struct filtkey {
   char csifiltk[44];                  /* Generic filter key          */
   char csicatnm[44];                  /* Catalog Name                */
   char csiresnm[44];                  /* Resume Name                 */
   char csidtypd[16];                  /* Entry Types                 */
                                       /* CSI Options                 */
   char csicldi;                       /*    data/index option        */
   char csiresum;                      /*    Resume indicator (set by */
                                       /*       IGGCSI00)             */
   char csis1cat;                      /*    Y=Only 1 catalog srched  */
   char csioptns;                      /*    fullword/halfword option */
   unsigned short csinumen;            /*    Y=Only 1 catalog srched  */
   char csifldnm[8];                   /* Field Name                  */
} filtkey;

/* --- Reason Code Structure ---------------------------------------- */
typedef struct reascde {
   char reasmod[2];                    /* 2 byte module ID            */
   char reasrc;                        /* Return code                 */
   char reasreas;                      /* Reason code                 */
} reascde;

/* --- Workarea Header ---------------------------------------------- */
typedef struct wrkhdr {
   int  csiusrln;                      /* Length of workarea          */
   int  csireqln;                      /* Miminum len for 1 entry     */
   int  csiusdln;                      /* Length of used workarea     */
   short csinumfd;                     /* Number fields + 1           */
} wrkhdr;

/* --- Workarea Entry ----------------------------------------------- */
typedef struct wrkent {
   char csieflag;                      /* Entry flags                 */
   char csietype;                      /* Entry Type                  */
   char csiename[44];                  /* Entry name                  */
   char csieretm[2];                   /* Entry return module         */
   char csieretr;                      /* Entry return reason code    */
   char csieretc;                      /* Entry return code           */
                                       /* Returned Data               */
   short csiedata_len;                 /*    Field Length             */
   char csiedata[6];                   /*    Field data. In our case, */
                                       /*    the volume serial.       */
                                       /*    Volsers are always 6     */
                                       /*    characters               */
} wrkent;

/* ================================================================== */
/* Variables and Other Definitions                                    */
/* ================================================================== */
/* --- Define the IGGCSI00 Program as External With OS Parms -------- */
extern int iggcsi00(
   reascde * __iggrsn, /* Input - fullword for reason code*/
   struct filtkey * __iggfield,  /* Input - selection criteria fields */
   void * __iggwork);   /* Output - workarea */
#pragma map(iggcsi00, "IGGCSI00")

/* --- Variables ---------------------------------------------------- */
int rc=0;                              /* Return Code                 */
char * str1, * str2, * str3;           /* Output buffer               */

reascde igg_reas;                      /* reason code from IGGCSI00   */
reascde * ptr_reas;                    /* Ptr to reason code          */

char * work1;                          /* IGGCSI00 workarea           */
filtkey * filt1;                       /* Filter area pointer         */

const int worksize=64000;              /* Workarea size - IBM         */
                                       /* recommends 64000            */
wrkhdr * wrkhdr1;                      /* Workarea header pointer     */
wrkent * wrkent1;                      /* Workarea dataset entry      */
int wrkpos;                            /* Position on workarea        */

int usdwrk;                            /* Amount workarea used        */
char more='Y';                         /* More to process?            */

static int first=1;                    /* for repeated calls          */
FILE *f1;                              /* for listing output          */
fpos_t start;                          /* to mark the start of file   */
/* ================================================================== */
/* dsn1;  dataset name to either search,delete or rename;             */
/* arg2;  dataset operation. GET,REN or DEL... GET is the default!    */
/* arg3;  MHS type of dataset. MHS% , where % is the single char arg. */
/* arg4;  MHS dataset owner. E.g BUF1                                 */
/* retcd; returns an integer to the calling program.                  */
/* ================================================================== */
int mhsiffc(char *dsn1,char *arg2,char *arg3,char *arg4,int *retcd) {
   /* --------------------------------------------------------------- */
   /* Initialise areas                                                */
   /* --------------------------------------------------------------- */
   int numdset=0;                         /* Number of datasets       */
   char dsnfil[46]= "MHS";
   char dsnren[46]= "'";
   char ren[5]= "'MHS";
   char isren[8] = "REN";
   char isdel[8] = "DEL";
   char isget[8] = "GET";
   if (first == 1) {
/*   f1 = fopen("DD:SYSTEMP","w+b");          */
     f1 = tmpfile();
     if (f1 != NULL) {
         if (fgetpos(f1,&start)) perror("Saving file position");
     } else {
         perror("Open Error in MHSIFFT");
     }
   }

/*------------------------------------------------------------------- */
/* Test whether we have a VALID MHS dataset for rename and deletes    */
/*------------------------------------------------------------------- */
   int isValid = strncmp(dsnfil, dsn1, 3 );

   strcat(dsnfil,arg3);
   strcat(dsnfil,".*.*.");
   strcat(dsnfil,arg4);
   strcat(dsnfil,".*");

   int length = strlen(dsnfil);
   int l1 = strlen(arg2);

/*------------------------------------------------------------------- */
/* Test whether we have been instructed to rename MHS dataset         */
/*------------------------------------------------------------------- */
   int cp = strncmp(isren, arg2, 3 );
   if (cp == 0 && isValid==0) {
     strncat(dsnren,dsn1,44);
     strcat(dsnren,"'");
     strcpy(dsnfil,dsnren);
     strcat(ren,arg3);
     strncpy(dsnfil,ren,5);
/*   rc = rename(dsnren,dsnfil);  NO renames in this version */
     *retcd = 0;
/*   printf("Rename %s with %s rc=%d\n" , dsnren, dsnfil, rc); */
   return(rc);
   }
/*------------------------------------------------------------------- */
/* Test whether we have been instructed to delete/remove MHS dataset  */
/*------------------------------------------------------------------- */
   cp = strncmp(isdel, arg2, 3 );
   if (cp == 0 && isValid==0) {
     /* re-using dsnren to remove */
     strncat(dsnren,dsn1,44);
     strcat(dsnren,"'");
/*   rc = remove(dsnren);   NO deletes in this version */
     *retcd = 0;
/*   printf("Deleted %s with rc=%d\n" , dsnren, rc);  */
     return(rc);
   }
/*------------------------------------------------------------------- */
/* If not delete or rename then assume catalogue search required.     */
/*------------------------------------------------------------------- */
   rc = strncmp(isget, arg2, 3 );
   if (rc == 0) {

     if (first == 1) {
   str1= (char *)malloc(80);           /* Area for printing strings   */
   memset(str1,0,sizeof(str1));        /* Zero output buffer          */
   str2= (char *)malloc(80);           /* Area for printing strings   */
   memset(str2,0,sizeof(str2));        /* Zero output buffer          */
   str3= (char *)malloc(80);           /* Area for outputting strings */
   memset(str3,0,sizeof(str3));        /* Zero output buffer          */
   /* --------------------------------------------------------------- */
   /* Setup parameters to IGGCSI00                                    */
   /*    Here we're setting a constant datasetname pattern to.        */
   /*    This will return all datasets!!!!                            */
   /*    We're also setting one field: VOLSER. So we will get         */
   /*    Volume Serial number information for each dataset            */
   /* --------------------------------------------------------------- */
   ptr_reas = &igg_reas;               /* Set ptr for reason info     */
   work1= (char *)malloc(worksize);    /* IGGCSI00 workarea           */
   wrkhdr1 = (wrkhdr *)work1;          /* Ptr to workarea             */
   filt1= (filtkey *)malloc(sizeof(filtkey)); /* Filter Key pointer   */
   memset(filt1,' ',sizeof(filtkey));  /* Set filter to spaces        */
   memcpy(filt1->csifiltk,dsnfil,length);    /* Set Filter Key        */
   filt1->csinumen = 1;                /* One field                   */
   memcpy(filt1->csifldnm,"VOLSER  ",8); /* Get volser field          */
   wrkhdr1->csiusrln=worksize;         /* Workarea size               */

   /* --------------------------------------------------------------- */
   /* Print Out a Heading Line.                                       */
   /* --------------------------------------------------------------- */
   /*                                                                 */
   /* printf("Type    Dataset Name                              ");  */
   /* printf(" Volume\n");                                           */
   /* printf("---- -------------------------------------------- ");  */
   /* printf("--------\n");

   /* --------------------------------------------------------------- */
   /* Call IGGCSI00 Until there's an error, or there's no more        */
   /* --------------------------------------------------------------- */
   while (more == 'Y' && rc==00) {
      rc = iggcsi00(ptr_reas,filt1,work1);
      more = filt1->csiresum;          /* Is there more output?    */

      /* ------------------------------------------------------------ */
      /* If IGGCSI00 went OK, output every dataset.                   */
      /*    IGGCSI00 will output one entry for every dataset, and one */
      /*    entry for every catalog. We only want the dataset names.  */
      /* ------------------------------------------------------------ */
      if (rc == 0) {
         /* --- Called IGGCSI00 successfully ------------------------ */
         usdwrk=wrkhdr1->csiusdln;        /* Amount of workarea used  */
         wrkpos = 14;                     /* Start position           */

         /* --------------------------------------------------------- */
         /* Search through every entry provided in our workarea.      */
         /*   The workarea conssists of a 14 byte header, and then    */
         /*   Entries. The length depends on the entry:               */
         /*      Catalog Entries - 50 bytes.                          */
         /*      Dataset Entries - 50 bytes+field data.               */
         /* --------------------------------------------------------- */
         /* --- Get Every Entry and Output It ----------------------- */
         while (wrkpos < usdwrk ) {

            wrkent1 = (wrkent *)(work1+wrkpos); /* Map this entry   */

            if (wrkent1->csietype != '0') {
               /* --- Not a catalog entry --------------------------- */
               memcpy(str1,wrkent1->csiename,44);
               wrkpos = wrkpos+50;     /* Move ptr over entry         */

               /* --------------------------------------------------- */
               /* Here we check if we have our field data. In this    */
               /* example we've only requested one field: volser.     */
               /* We check there is no error, and the field length    */
               /* is greater than 0.                                  */
               /* Lengths are halfwords (short) because we specified  */
               /* a space in the CSIOPTNs field in the filter key.    */
               /* --------------------------------------------------- */
               if (wrkent1->csiedata_len >0 ) {
                  /* --- Len = 0, have our volume serial number ----- */
                  memcpy(str2,wrkent1->csiedata,
                     wrkent1->csiedata_len);
               }                       /* If have a volser            */
               wrkpos = wrkpos + wrkent1->csiedata_len + 2;
                                       /* Skip over volser field      */

         /* printf("(%c)  %s (%s)\n", wrkent1->csietype,str1, str2); */
               /* --------------------------------------------------- */
               /* Here we're printing out the dataset type. This      */
               /* could be:                                           */
               /*    A - Non-VSAM dataset                             */
               /*    B - GDG                                          */
               /*    C - VSAM Cluster                                 */
               /*    D - VSAM Data Component                          */
               /*    G - VSAM Alternate Index                         */
               /*    H - Generation Dataset                           */
               /*    I - VSAM Index Component                         */
               /*    R - Path                                         */
               /*    X - Alias                                        */
               /*    U - User Catalog Connector Entry                 */
               /*    L - ATL Library entry                            */
               /*    W - ATL Volume entry                             */
               /* --------------------------------------------------- */

               if (numdset == 0 ) {
                 strcpy(dsn1,str1);
                 int len = strlen(dsn1);
               } else {
                 strncpy(str3,str1,79);
                 strcat(str3,"\n");
                 fputs(str3,f1);
               }
/*             printf("DSN1  =%s with size=%d\n", dsn1,len);          */
/*             memset(str1,0,sizeof(str1));    Zero output buffer     */
/*             memset(str2,0,sizeof(str2));    Zero output buffer     */
               numdset++;                 /* Increment dataset count  */

            } else {

               /* --- Catalog Entry --------------------------------- */
               wrkpos = wrkpos + 50;      /* Skip to next entry       */

            }                             /* else                     */
         }                                /* do while more in wrkarea */
      } else {                            /* if rc=0                  */
         /* --- Had an error, so output the information ------------- */
         printf("IGGCSI00 rc = %d, reas= %d (reas) / %d (retc)\n", rc,
            igg_reas.reasrc, igg_reas.reasreas);
      }                                /* else                        */

   }                                   /* IF there is a GET           */
     first = 0;
     if (fsetpos(f1,&start)) perror("going back to the start");
   } else {    /* not a first time call */
     if ( fgets(str1,80,f1) != NULL) {
       strcpy(dsn1,str1);
       int len = strlen(dsn1);
       numdset= 1;
     } else {
       printf("end of MHS filelist\n");
       numdset=0;
       fclose(f1);
       more='Y';
       first = 1;       /* so that it can be called again with a     */
                        /* different owner                           */
     }
   }        /* end of subsequent call section                        */
   }
   free(str1);
   free(str2);
   free(str3);
   free(work1);
   free(filt1);
   if (rc == 0) {
      /* --- No errors, so output number of datasets ---------------- */
   /* printf(" \n");                                                  */
   /* printf(" %d Datasets Found\n", numdset);                        */
      *retcd = numdset;
   }                                   /* if rc = 0                   */

}                                      /* main                        */
