#define _LARGE_FILES 1
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

/*====================================================================*
 * MetDB_c_utils.c
 *
 * Routines metdb_copen      - open dataset
 *          metdb_cread      - read char data - sequential
 *          metdb_cwrite     - write char data - sequential
 *          metdb_cread_int  - read int data - sequential
 *          metdb_cwrite_int - write int data - sequential
 *          metdb_cread_dir  - read char data - direct access
 *          metdb_cwrite_dir - write char data - direct access
 *          metdb_cclose     - close dataset
 *          metdb_getenv     - get environment variable
 *
 * See individual routine headers for argument lists.
 *
 * NOTE: When compiling this code, the following options are
 *       available:
 *
 *       -DL64         : all integers and reals are 64 bit.
 *       -DUNDERSCORE  : routines have an underscore suffix.
 *       -DDUNDERSCORE : routines have a double underscore suffix.
 *
 * $Revision: 4$
 * $Date: 20/09/2011 10:10:22$
 * $Source: /home/us0400/mdb/op/lib/source/RCS/MetDB_c_utils.c,v $
 *
 * $Log:
 *  4    Met_DB_Project 1.3         20/09/2011 10:10:22    Sheila Needham
 *       Merged with Refresh version
 *  3    Met_DB_Project 1.2         24/09/2008 14:22:28    Richard Weedon
 *       Correction to metdbcclose
 *  2    Met_DB_Project 1.1         24/09/2008 13:49:23    Richard Weedon
 *       Correction to metdb_cclose
 *  1    Met_DB_Project 1.0         30/01/2006 20:23:22    Sheila Needham  
 * $
 * Revision 1.11  2008/09/24 14:30
 * Correction to metdb_cclose
 *
 * Revision 1.10  2003/04/10 09:08:12  usmdb
 * Changes made to include preprocessor statements L64,
 * DUNDERSCORE and UNDERSCORE. S.Cox
 *
 * Revision 1.9  2002/01/08 10:02:22  usmdb
 * New routines metdb_cread_int and metdb_cwrite_int to read and
 * write integer data - S.Cox
 *
 * Revision 1.8  2001/08/22 09:33:53  usmdb
 * A 3rd version of each routine made - with 2 underscores
 * suffixed - S.Cox
 *
 * Revision 1.7  2001/03/08 09:26:02  usmdb
 * Added copyright - S.Cox
 *
 * Revision 1.6  2001/02/15 13:29:41  usmdb
 * Repeated each routine with an underscore suffixed. This
 * if for f90/f95 calling programs. The original is retained
 * for f77 calling - S.Cox
 *
 * Revision 1.5  2001/02/13 10:50:22  usmdb
 * Improvements. use fopen, fread, fwrite, flcose rather than
 * unbuffered I/O. Improvements to metdb_copen so user can
 * specify file mode. S.Cox

 * Revision 1.3  2000/11/08  14:43:56  14:43:56  usmdb (Generic MDB account)
 * Addition of NOTE regarding f90 compilation of calling
 * code - S.Cox
 *
 * Revision 1.2  2000/11/03  11:37:07  11:37:07  usmdb (Generic MDB account)
 * Corrected metdb_copen routine - S.Cox
 *
 * Revision 1.1  2000/08/25  14:18:57  14:18:57  usmdb (Generic MDB account)
 * Initial revision
 *
 * Revision 1.2  99/04/22  12:34:37  12:34:37  usmdb (Generic MDB account)
 * Add cwrite - S.Cox
 *
 * Revision 1.1  99/04/22  12:32:55  12:32:55  usmdb (Generic MDB account)
 * Initial revision
 *
 *====================================================================*
 * (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *====================================================================*/

#if defined (L64)
typedef double real;
typedef long integer;
#else
typedef float real;
typedef int integer;
#endif

#define MAX_UNITS 100

FILE *fs[MAX_UNITS] = {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
                       NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

integer fo[MAX_UNITS] = {0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0};

  off_t    offset;
/*====================================================================
 * METDB_COPEN ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *
 *        i/p  fdataset    : file name. This must be appended with
 *                           CHAR(0). e.g.
 *                           fdataset = 'file.binary'//CHAR0
 *
 *        i/p  mode        : file open mode.
 *                            0 = read only "rb"
 *                            1 = read and append "a+b"
 *                            2 = read and write new "w+b"
 *                            3 = read and write update r+b
 *
 *        o/p  rc          :  0 = open successful.
 *                           -1 = error in open. File doesn't exist or
 *                                permissions don't allow open.
 *                           -2 = file already open! close 1st
 *                           -3 = unit out of range
 *
 *    ignore  len_dataset  : generated by Fortran interface
 *
 *====================================================================*/

#if defined (DUNDERSCORE)			
void metdb_copen__(unit,fdataset,mode,rc,len_dataset)
#elif defined (UNDERSCORE)
void metdb_copen_(unit,fdataset,mode,rc,len_dataset)
#else
void metdb_copen(unit,fdataset,mode,rc,len_dataset)
#endif	

integer *unit;
char    *fdataset;
integer *mode;
integer *rc;
integer len_dataset;
{
  char *cdataset;
  FILE *f1;
  char cmode[4];

/*--------------------------------------------------------------------
  Check input unit number is in range.
  --------------------------------------------------------------------*/

  if (*unit < 0 || *unit > (MAX_UNITS-1)) {
    perror("Unit Error in MetDB_copen");
    *rc = -3;
    return;
  }

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so return -2 to calling
  program.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    *rc = -2;
    return;
  }

  cdataset = malloc(len_dataset + 1);
  strncpy(cdataset,fdataset,len_dataset);
  cdataset[len_dataset] = '\0';

  if (*mode == 0)
    strncpy(cmode,"rb\0",3);   /* read only */
  if (*mode == 1)
    strncpy(cmode,"a+b\0",4);  /* read and append */
  if (*mode == 2)
    strncpy(cmode,"w+b\0",4);  /* read and write new */
  if (*mode == 3)
    strncpy(cmode,"r+b\0",4);  /* read and write new */

  f1 = fopen(cdataset,cmode);
  if (f1 != NULL) {
    fo[*unit] = 1;
    fs[*unit] = f1;
    *rc = 0;                 /* file opened OK */
  } else {
    perror("Open Error in MetDB_copen");
    *rc = -1;
  }
  free(cdataset);

  return;
}

/*====================================================================
 * METDB_CREAD ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *
 *        i/p  buffer      : character string to read binary data into.
 *
 *        o/p  read_size   : amount of data read and put into buffer.
 *                           If 0, then most likely an error in read
 *                           or file empty.
 *
 *     ignore  lbuffer     : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cread__(unit,buffer,read_size,lbuffer)
#elif defined (UNDERSCORE)
void metdb_cread_(unit,buffer,read_size,lbuffer)
#else
void metdb_cread(unit,buffer,read_size,lbuffer)
#endif

integer *unit;
char    *buffer;
integer *read_size;
integer lbuffer;
{
  FILE *f1;

  *read_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to read from it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *read_size = fread(buffer,sizeof(char),lbuffer,f1);
  }

  return;
}

/*====================================================================
 * METDB_CWRITE ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *
 *        i/p  buffer      : character string to read binary data into.
 *
 *        o/p  write_size  : amount of data written from buffer to the
 *                         : file. If 0, then an error occurred.
 *
 *     ignore  lbuffer     : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cwrite__(unit,buffer,write_size,lbuffer)
#elif defined (UNDERSCORE)
void metdb_cwrite_(unit,buffer,write_size,lbuffer)
#else
void metdb_cwrite(unit,buffer,write_size,lbuffer)
#endif

integer *unit;
char    *buffer;
integer *write_size;
integer lbuffer;
{
  FILE *f1;

  *write_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to write to it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *write_size = fwrite(buffer,sizeof(char),lbuffer,f1);
  }

  return;
}

/*====================================================================
 * METDB_CREAD_INT ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *
 *        i/p  buffer      : integer array to read binary data into.
 *
 *        i/p  buffer_size : size of integer array (buffer).
 *
 *        o/p  read_size   : amount of data read and put into buffer.
 *                           If 0, then most likely an error in read
 *                           or file empty.
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cread_int__(unit,buffer,buffer_size,read_size)
#elif defined (UNDERSCORE)
void metdb_cread_int_(unit,buffer,buffer_size,read_size)
#else
void metdb_cread_int(unit,buffer,buffer_size,read_size)
#endif

integer *unit;
integer *buffer;
integer *buffer_size;
integer *read_size;
{
  FILE *f1;

  *read_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to read from it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *read_size = fread(buffer,sizeof(integer),*buffer_size,f1);
  }

  return;
}

/*====================================================================
 * METDB_CWRITE_INT ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *
 *        i/p  buffer      : character string to read binary data into.
 *
 *        i/p  buffer_size : size of integer array (buffer).
 *
 *        o/p  write_size  : amount of data written from buffer to the
 *                         : file. If 0, then an error occurred.
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cwrite_int__(unit,buffer,buffer_size,write_size)
#elif defined (UNDERSCORE)
void metdb_cwrite_int_(unit,buffer,buffer_size,write_size)
#else
void metdb_cwrite_int(unit,buffer,buffer_size,write_size)
#endif
	
int  *unit;
int  *buffer;
int  *buffer_size;
int  *write_size;
{
  FILE *f1;

  *write_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to write to it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *write_size = fwrite(buffer,sizeof(integer),*buffer_size,f1);
  }

  return;
}
/*====================================================================
 * METDB_CREAD_DIR ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i    unit        : file unit number (0 - 99).
 *
 *        i/o  buffer      : character string to read binary data into.
 *
 *        i    blksize     : file blksize.
 *
 *        i    recno       : record number (starting at 1)
 *
 *        o    rc          : return code, -3 invalid recno
 *                                        -4 read error
 *
 *     ignore  lbuffer     : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cread_dir__(unit,buffer,blksize,recno,rc,lbuffer)
#elif defined (UNDERSCORE)
void metdb_cread_dir_(unit,buffer,blksize,recno,rc,lbuffer)
#else
void metdb_cread_dir(unit,buffer,blksize,recno,rc,lbuffer)
#endif

integer *unit;
char    *buffer;
integer *blksize;
integer *recno;
integer *rc;
integer lbuffer;
{
  FILE *f1;
  *rc = 0;
  off_t blk;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to read from it.
  --------------------------------------------------------------------*/
  blk = (off_t)*blksize;
  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    offset = (off_t)(*recno - 1) * blk;
    if (fseeko(f1, offset, SEEK_SET) != 0 ) {
       perror("Error in MetDB_cread_dir");
       *rc = -3;
       return;
    } else {

       if (fread (buffer, sizeof(char), blk, f1) != blk ) {
          perror("Error in MetDB_cread_dir");
          *rc = -4;
          return;
       }
    }

  }

  return;
}

/*====================================================================
 * METDB_CWRITE_DIR ROUTINE.
 *
 * ARGUMENTS :
 *        i    unit        : file unit number (0 - 99).
 *
 *        i/o  buffer      : character string to write out.
 *
 *        i    blksize     : file blksize.
 *
 *        i    recno       : record number (starting at 1)
 *
 *        o    rc          : return code, -3 invalid recno
 *                                        -4 write error
 *     ignore  lbuffer     : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cwrite_dir__(unit,buffer,blksize,recno,rc,lbuffer)
#elif defined (UNDERSCORE)
void metdb_cwrite_dir_(unit,buffer,blksize,recno,rc,lbuffer)
#else
void metdb_cwrite_dir(unit,buffer,blksize,recno,rc,lbuffer)
#endif

integer *unit;
char    *buffer;
integer *blksize;
integer *recno;
integer *rc;
integer lbuffer;
{
  FILE *f1;
  off_t blk;

  *rc = 0;
  blk = (off_t)*blksize;
/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to write to it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    offset = (off_t)(*recno - 1) * blk;
    if (fseeko(f1, offset, SEEK_SET) != 0 ) {
       perror("Error in MetDB_cwrite_dir");
       *rc = -3;
       return;
    } else {

       if (fwrite (buffer, sizeof(char), blk, f1) != blk) {
          perror("Error in MetDB_cwrite_dir");
          *rc = -4;
          return;
       }
    }
  }

  return;
}
/*====================================================================
 * METDB_CCLOSE ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  unit        : file unit number (0 - 99).
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_cclose__(unit)
#elif defined (UNDERSCORE)
void metdb_cclose_(unit)
#else
void metdb_cclose(unit)
#endif

integer *unit;

{
  FILE *f1;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to close it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    fclose(f1);
    fs[*unit] = NULL;
    fo[*unit] = 0;
  }

  return;
}

/*====================================================================
 * METDB_GETENV ROUTINE.
 *
 * ARGUMENTS :
 *
 *        i/p  f_in_env    : environment variable name
 *
 *        o/p  f_out       : contents of environment variable
 *
 *        o/p  rc          :  0 = success
 *                           -1 = error getting contents of environment
 *                                variable.
 *                           -2 = not enough room in f_out to hold
 *                                contents of environment variable
 *
 *     ignore  len_in_env  : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *
 *     ignore  len_f_out   : not passed by calling program. Used
 *                           internally for FORTRAN to C
 *                           interlanguage call.
 *====================================================================*/

#if defined (DUNDERSCORE)
void metdb_getenv__(f_in_env,f_out,rc,len_in_env,len_f_out)
#elif defined (UNDERSCORE)
void metdb_getenv_(f_in_env,f_out,rc,len_in_env,len_f_out)
#else
void metdb_getenv(f_in_env,f_out,rc,len_in_env,len_f_out)
#endif

char    *f_in_env;
char    *f_out;
integer *rc;
integer len_in_env;
integer len_f_out;
{
  char *c_in_env;
  char *c_out;
  int  i;

  *rc = 0;

  c_in_env = malloc(len_in_env + 1);
  strncpy(c_in_env,f_in_env,len_in_env);
  c_in_env[len_in_env] = '\0';
  c_out = getenv(c_in_env);
  if (c_out == NULL) {
    *rc = -1;
    free(c_in_env);
    return;
  }

  if (strlen(c_out) > len_f_out) {
    (void)fprintf(stdout,"metdb_getenv: Not enough room to hold " \
                  "contents of env var %s\n",c_in_env);
    *rc = -2;
    free(c_in_env);
    return;
  }
  strncpy(f_out,c_out,strlen(c_out));

  for (i=strlen(c_out); i<len_f_out; i=i+1) {
    f_out[i] = ' ';
  }

  free(c_in_env);
  return;

}

/*====================================================================*/
