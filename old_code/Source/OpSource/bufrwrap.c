/*----------------------------------------------------------------*
 * Program      : bufrwrap.c
 *
 * Language     : C
 *
 * Description  : Wrappers around the F95 BUFRDLL entry point routines
 *                to initialise the NAG Fortran environment.
 *                No #program linkage directive here since C and F95
 *                use the same format for passing character arguments.
 *                Routines are:
 *                  debufr_wrap
 *                  desfxy_wrap
 *                  code_wrap
 *                  tabled_wrap
 *                  enbufr_wrap
 *                  ides_wrap
 *                  locald_wrap
 *                  enbufv4_wrap
 *
 * Called by    : bufrshel.c routines
 *
 * Calls        : BUFR routines in BUFRDLL
 *
 * Arguments    : as for BUFR routines but with extra integers giving
 *                the lengths of the character variables passed where
 *                applicable.
 *
 * $Workfile: bufrwrap.c$  $Folder: OpSource$
 * $Revision: 2$  $Date: 30/01/2012 18:39:34$
 *
 * Changes      :
 * $Log:
 *  2    MetDB_Refresh 1.1         30/01/2012 18:39:34    Sheila Needham
 *       Updated header file for f2003
 *  1    MetDB_Refresh 1.0         21/02/2011 11:24:29    Alison Weir
 *       BUFRDLL wrappers
 * $
 *
 *-----------------------------------------------------------------
 * (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom.
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-----------------------------------------------------------------*/

#pragma runopts(POSIX(ON))

#include <stdio.h>
#include <nagfortran.h>

/* ------------------------------------------------------------ */
/*      debufr                                                  */
/* ------------------------------------------------------------ */

extern int debufr(Integer *descr_,
                  Real *values_,
                  char *names_,
                  Integer *nd_,
                  Integer *nobs_,
                  char *string_,
                  Logical *dsplay_,
                  int names_Len,
                  int string_Len);

int debufr_wrap(Integer *descr_,
                  Real *values_,
                  char *names_,
                  Integer *nd_,
                  Integer *nobs_,
                  char *string_,
                  Logical *dsplay_,
                  int names_Len,
                  int string_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call DEBUFR                     */

  debufr_(descr_,
          values_,
          names_,
          nd_,
          nobs_,
          string_,
          dsplay_,
          names_Len,
          string_Len);
  return 0;
}

/* ------------------------------------------------------------ */
/*      desfxy                                                  *
/* ------------------------------------------------------------ */
extern int desfxy(Integer *descr_,
                  Integer *f_,
                  Integer *x_,
                  Integer *y_);

int desfxy_wrap(Integer *descr_,
                  Integer *f_,
                  Integer *x_,
                  Integer *y_) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call DESFXY */
  desfxy_(descr_, f_, x_, y_);
  return 0;
}

/* ------------------------------------------------------------ */
/*      code                                                    *
/* ------------------------------------------------------------ */
extern int code(Integer *descr_,
                Integer *figure_,
                char    *words_,
                int     words_Len);

int code_wrap(Integer *descr_,
              Integer *figure_,
              char    *words_,
              int     words_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call CODE */
  code_(descr_, figure_, words_, words_Len);
  return 0;
}

/* ------------------------------------------------------------ */
/*      tabled                                                  *
/* ------------------------------------------------------------ */
extern int tabled(Integer *xreq_,
                  Integer *yreq_,
                  Integer *seq_,
                  Integer *nd_);

int tabled_wrap(Integer *xreq_,
                  Integer *yreq_,
                  Integer *seq_,
                  Integer *nd_) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call TABLED */
  tabled_(xreq_, yreq_, seq_, nd_);
  return 0;
}

/* ------------------------------------------------------------ */
/*      enbufr                                                  *
/* ------------------------------------------------------------ */
extern int enbufr(Integer *descr_,
                  Real *values_,
                  Integer *nd_,
                  Integer *nelem_,
                  Integer *nobs_,
                  char *names_,
                  Integer *datime_,
                  char *mesage_,
                  Logical *cmpres_,
                  Integer *l_,
                  Integer *iver_,
                  int names_Len,
                  int mesage_Len);

int enbufr_wrap(Integer *descr_,
                Real *values_,
                Integer *nd_,
                Integer *nelem_,
                Integer *nobs_,
                char *names_,
                Integer *datime_,
                char *mesage_,
                Logical *cmpres_,
                Integer *l_,
                Integer *iver_,
                int names_Len,
                int mesage_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call ENBUFR */
  enbufr_(descr_,
          values_,
          nd_,
          nelem_,
          nobs_,
          names_,
          datime_,
          mesage_,
          cmpres_,
          l_,
          iver_,
          names_Len,
          mesage_Len);
  return 0;
}

/* ------------------------------------------------------------ */
/*      ides                                                    *
/* ------------------------------------------------------------ */
extern int ides(Integer *fxxyyy_);

int ides_wrap(Integer *fxxyyy_) {

int result;

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call IDES */
  result = ides_(fxxyyy_);
  return result;
}

/* ------------------------------------------------------------ */
/*      locald                                                  *
/* ------------------------------------------------------------ */
extern int locald(Integer *x_,
                  Integer *y_,
                  Integer *seq_,
                  Integer *nseq_,
                  char *text_,
                  char *mode_,
                  int text_Len,
                  int mode_Len);

int locald_wrap(Integer *x_,
                Integer *y_,
                Integer *seq_,
                Integer *nseq_,
                char *text_,
                char *mode_,
                int text_Len,
                int mode_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call LOCALD */
  locald_(x_,
          y_,
          seq_,
          nseq_,
          text_,
          mode_,
          text_Len,
          mode_Len);
  return 0;
}

/* ------------------------------------------------------------ */
/*      enbufv4                                                 *
/* ------------------------------------------------------------ */
extern int enbufv4(Integer *descr_,
                   Real    *values_,
                   Integer *nd_,
                   Integer *nelem_,
                   Integer *nobs_,
                   char    *names_,
                   Integer *datime_,
                   char    *mesage_,
                   Logical *cmpres_,
                   Integer *l_,
                   Integer *edition_,
                   Integer *mastertable_,
                   Integer *vermastab_,
                   Integer *origcentre_,
                   Integer *origsubcentre_,
                   Integer *datatype_,
                   Integer *localdatasubtype_,
                   Integer *intdatasubtype_,
                   Integer *verloctab_,
                   Logical *extrasect1_,
                   char    *charsect1_,
                   Logical *extrasect2_,
                   char    *charsect2_,
                   Integer *sect3type_,
                   int     names_Len,
                   int     mesage_Len,
                   int     charsect1_Len,
                   int     charsect2_Len);

int enbufv4_wrap(Integer *descr_,
                 Real    *values_,
                 Integer *nd_,
                 Integer *nelem_,
                 Integer *nobs_,
                 char    *names_,
                 Integer *datime_,
                 char    *mesage_,
                 Logical *cmpres_,
                 Integer *l_,
                 Integer *edition_,
                 Integer *mastertable_,
                 Integer *vermastab_,
                 Integer *origcentre_,
                 Integer *origsubcentre_,
                 Integer *datatype_,
                 Integer *localdatasubtype_,
                 Integer *intdatasubtype_,
                 Integer *verloctab_,
                 Logical *extrasect1_,
                 char    *charsect1_,
                 Logical *extrasect2_,
                 char    *charsect2_,
                 Integer *sect3type_,
                 int     names_Len,
                 int     mesage_Len,
                 int     charsect1_Len,
                 int     charsect2_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call ENBUFV4 */
  enbufv4_(descr_,
           values_,
           nd_,
           nelem_,
           nobs_,
           names_,
           datime_,
           mesage_,
           cmpres_,
           l_,
           edition_,
           mastertable_,
           vermastab_,
           origcentre_,
           origsubcentre_,
           datatype_,
           localdatasubtype_,
           intdatasubtype_,
           verloctab_,
           extrasect1_,
           charsect1_,
           extrasect2_,
           charsect2_,
           sect3type_,
           names_Len,
           mesage_Len,
           charsect1_Len,
           charsect2_Len);
  return 0;
}

