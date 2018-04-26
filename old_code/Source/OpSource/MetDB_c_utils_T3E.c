#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fortran.h>

/*====================================================================*
 * MetDB_c_utils_T3E.c
 *
 * T3E version
 *
 * Routines metdb_copen      - open dataset
 *          metdb_cread      - read data
 *          metdb_cwrite     - write data
 *          metdb_cread_int  - read int data
 *          metdb_cwrite_int - write int data
 *          metdb_cclose     - close dataset
 *          metdb_getenv     - get environment variable
 *
 * See individual routine headers for argument lists.
 *
 * $Revision: 1$
 * $Date: 30/01/2006 20:23:23$
 * $Source: /home/us0400/mdb/op/lib/source/RCS/MetDB_c_utils_T3E.c,v $
 *
 * $Log:
 *  1    Met_DB_Project 1.0         30/01/2006 20:23:23    Sheila Needham  
 * $
 * Revision 1.4  2002/01/25 11:57:36  usmdb
 * Added METDB_CWRITE_INT and METDB_CREAD_INT - S.Cox
 *
 * Revision 1.3  2001/03/08 09:26:02  usmdb
 * Added copyright - S.Cox
 *
 * Revision 1.2  2001/02/13 10:52:18  usmdb
 * Improvements. use fopen, fread, fwrite, flcose rather than
 * unbuffered I/O. Improvements to metdb_copen so user can
 * specify file mode. S.Cox
 *
 * Revision 1.1  2000/08/25  14:18:58  14:18:58  usmdb (Generic MDB account)
 * Initial revision
 * 
 *====================================================================*
 * (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database 
 * Team at the above address.
 *====================================================================*/

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

int fo[MAX_UNITS] = {0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0,
                     0,0,0,0,0,0,0,0,0,0};

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
 *
 *        o/p  rc          :  0 = open successful.
 *                           -1 = error in open. File doesn't exist or
 *                                permissions don't allow open.
 *                           -2 = file already open! close 1st
 *                           -3 = unit out of range
 *====================================================================*/

void METDB_COPEN(unit,fdataset,mode,rc)
  
int  *unit;
_fcd fdataset;
int  *mode;
int  *rc;
{
  FILE *f1;
  char cmode[3];
  int  len_dataset;
  char *cdataset;
  
/*--------------------------------------------------------------------
  Check input unit number is in range.
  --------------------------------------------------------------------*/

  if (*unit < 0 || *unit > (MAX_UNITS-1)) {
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

  *rc = -1;     /* set error return code in case of failure */

  len_dataset = _fcdlen(fdataset);
  cdataset = malloc(len_dataset + 1);
  strncpy(cdataset,_fcdtocp(fdataset),len_dataset);
  cdataset[len_dataset] = '\0';

  if (*mode == 0)
    strncpy(cmode,"rb",2);   /* read only */
  if (*mode == 1)
    strncpy(cmode,"a+b",3);  /* read and append */
  if (*mode == 2)
    strncpy(cmode,"w+b",3);  /* read and write new */
  
  f1 = fopen(cdataset,cmode);
  if (f1 != NULL) {
    fo[*unit] = 1;
    fs[*unit] = f1;
    *rc = 0;                 /* file opened OK */
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
 *        o/p  read_size   : amount of data read andput into buffer.
 *                           If 0, then most likely an error in read
 *                           or file empty.
 *====================================================================*/

void METDB_CREAD(unit,buffer,read_size)

int  *unit;
_fcd buffer;
int  *read_size;
{
  FILE *f1;

  *read_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to read from it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *read_size = fread(_fcdtocp(buffer),sizeof(char),_fcdlen(buffer),f1);
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
 *====================================================================*/

void METDB_CWRITE(unit,buffer,write_size)

int  *unit;
_fcd buffer;
int  *write_size;
{
  FILE *f1;

  *write_size = 0;
  
/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to write to it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *write_size = fwrite(_fcdtocp(buffer),sizeof(char),_fcdlen(buffer),f1);
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

void METDB_CREAD_INT(unit,buffer,buffer_size,read_size)

int  *unit;
int  *buffer;
int  *buffer_size;
int  *read_size;
{
  FILE *f1;

  *read_size = 0;

/*--------------------------------------------------------------------
  See if file is already open (fo = 1). If so, OK to read from it.
  --------------------------------------------------------------------*/

  if (fo[*unit] == 1) {
    f1 = fs[*unit];
    *read_size = fread(buffer,sizeof(int),*buffer_size,f1);
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

void METDB_CWRITE_INT(unit,buffer,buffer_size,write_size)

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
    *write_size = fwrite(buffer,sizeof(int),*buffer_size,f1);
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

void METDB_CCLOSE(unit)

int *unit;

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
 *====================================================================*/

void METDB_GETENV(f_in_env,f_out,rc)

_fcd f_in_env;
_fcd f_out;
int  *rc;
{
  char *c_in_env;
  char *c_out; 
  int  i;
 
  *rc = 0;

  c_in_env = malloc(_fcdlen(f_in_env) + 1);
  strncpy(c_in_env,_fcdtocp(f_in_env),_fcdlen(f_in_env));
  c_in_env[_fcdlen(f_in_env)] = '\0';
  c_out = getenv(c_in_env);
  if (c_out == NULL) {
    *rc = -1;
    free(c_in_env);
    return;
  }
  if (strlen(c_out) > _fcdlen(f_out)) {
    (void)fprintf(stdout,"metdb_getenv: Not enough room to hold " \
                  "contents of env var %s\n",c_in_env);
    *rc = -2;
    free(c_in_env);
    return;
  } 
  strncpy(_fcdtocp(f_out),c_out,strlen(c_out));

  for (i=strlen(c_out); i<_fcdlen(f_out); i=i+1) {
    _fcdtocp(f_out)[i] = ' ';
  }

  free(c_in_env);
  return;

}

/*====================================================================*/
