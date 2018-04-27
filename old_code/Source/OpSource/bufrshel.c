/*----------------------------------------------------------------*
 * Program      : bufrshel.c
 *
 * Language     : C
 *
 * Description  : Interfaces between fortran programs and BUFR DLL.
 *                #pragma linkage directive required for Fortran to
 *                C calls.
 *                There is a routine for each BUFRDLL entry point,
 *                currently :
 *                  DEBUFR - named DEBUFRC, as called from preprocessor
 *                  DESFXY
 *                  CODE - named CODEC, as called from preprocessor
 *                  TABLED
 *                  ENBUFR - named ENBUFRC, as called from preprocessor
 *                  IDES
 *                  LOCALD - named LOCALDC, as called from preprocessor
 *                  ENBUFV4 - named ENBUF4C, as called from preprocessor
 *                            (also, name had to be shortened, so no V)
 *                The routine name varies depending on whether it is
 *                called from VSFortran 77 or Fortran 90/95, hence the
 *                '#if defined F95' statements. For f90/95, compile with
 *                the -DF95 option.
 *
 * Called by    : MDBDLL, user programs and BUFR pre-processor
 *
 * Calls        : routines in bufrwrap.c - C interfaces to BUFR DLL
 *
 * Arguments    : as for the BUFR routines but with explicit lengths for
 *                all character variables passed.
 *
 * $Workfile: bufrshel.c$  $Folder: OpSource$
 * $Revision: 1$  $Date: 21/02/2011 11:24:16$
 *
 * Changes      :
 * $Log:
 *  1    MetDB_Refresh 1.0         21/02/2011 11:24:16    Alison Weir
 *       BUFRSHEL DLL loading routines
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

#if defined F95
#pragma linkage(debufrc_,FORTRAN)
#pragma linkage(desfxy_,FORTRAN)
#pragma linkage(codec_,FORTRAN)
#pragma linkage(tabled_,FORTRAN)
#pragma linkage(enbufrc_,FORTRAN)
#pragma linkage(ides_,FORTRAN)
#pragma linkage(localdc_,FORTRAN)
#pragma linkage(enbuf4c_,FORTRAN)
#else
#pragma linkage(DEBUFRC,FORTRAN)
#pragma linkage(DESFXY,FORTRAN)
#pragma linkage(CODEC,FORTRAN)
#pragma linkage(TABLED,FORTRAN)
#pragma linkage(ENBUFRC,FORTRAN)
#pragma linkage(IDES,FORTRAN)
#pragma linkage(LOCALDC,FORTRAN)
#pragma linkage(ENBUF4C,FORTRAN)
#endif

#pragma runopts(POSIX(ON))

#include <stdio.h>
#include <f95.h>

/* declare functions to be called    */

extern int debufr_wrap(Integer *descr_,
                  Real *values_,
                  char *names_,
                  Integer *nd_,
                  Integer *nobs_,
                  char *string_,
                  Logical *dsplay_,
                  int names_Len,
                  int string_Len);

extern int desfxy_wrap(Integer *descr_,
                  Integer *f_,
                  Integer *x_,
                  Integer *y_);

extern int code_wrap(Integer *descr_,
                  Integer *figure_,
                  char    *words_,
                  int     words_Len);

extern int tabled_wrap(Integer *xreq_,
                  Integer *yreq_,
                  Integer *seq_,
                  Integer *nd_);

extern int enbufr_wrap(Integer *descr_,
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

extern int ides_wrap(Integer *fxxyyy_);

extern int locald_wrap(Integer *x_,
                       Integer *y_,
                       Integer *seq_,
                       Integer *nseq_,
                       char *text_,
                       char *mode_,
                       int text_Len,
                       int mode_Len);

extern int enbufv4_wrap(Integer *descr_,
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

/* Shell functions, name depends on calling program   */

/* -------------------------------------------------- *
 *   DEBUFR                                           *
 * -------------------------------------------------- */

#if defined F95
int debufrc_(Integer *descr_,
#else
int DEBUFRC(Integer *descr_,
#endif
                  Real *values_,
                  char *names_,
                  Integer *nd_,
                  Integer *nobs_,
                  char *string_,
                  Logical *dsplay_,
                  int names_L,
                  int string_L,
                  int names_Len,
                  int string_Len) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call DEBUFR via C wrapper.  This loads BUFRDLL (at entry point
   debufr_wrap.  */

debufr_wrap(descr_,
          values_,
          names_,
          nd_,
          nobs_,
          string_,
          dsplay_,
          names_L,
          string_L);

return 0;
}

/* -------------------------------------------------- *
 *   DESFXY                                           *
 * -------------------------------------------------- */

#if defined F95
int desfxy_(Integer *descr_,
#else
int DESFXY(Integer *descr_,
#endif
           Integer *f_,
           Integer *x_,
           Integer *y_) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call DESFXY via C wrapper.  This loads BUFRDLL (at entry point
   desfxy_wrap.  */

desfxy_wrap(descr_, f_, x_, y_);

return 0;
}

/* -------------------------------------------------- *
 *   CODE                                             *
 * -------------------------------------------------- */

#if defined F95
int codec_(Integer *descr_,
#else
int CODEC(Integer *descr_,
#endif
         Integer *figure_,
         char    *words_,
         int     words_L,
         int     words_Len) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call CODE via C wrapper.  This loads BUFRDLL (at entry point
   code_wrap.  */

code_wrap(descr_, figure_, words_, words_L);

return 0;
}

/* -------------------------------------------------- *
 *   TABLED                                           *
 * -------------------------------------------------- */

#if defined F95
int tabled_(Integer *xreq_,
#else
int TABLED(Integer *xreq_,
#endif
           Integer *yreq_,
           Integer *seq_,
           Integer *nd_) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call TABLED via C wrapper.  This loads BUFRDLL (at entry point
   tabled_wrap.  */

tabled_wrap(xreq_, yreq_, seq_, nd_);

return 0;
}

/* -------------------------------------------------- *
 *   ENBUFR                                           *
 * -------------------------------------------------- */

#if defined F95
int enbufrc_(Integer *descr_,
#else
int ENBUFRC(Integer *descr_,
#endif
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
            int names_L,
            int mesage_L,
            int names_Len,
            int mesage_Len) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call ENBUFR via C wrapper.  This loads BUFRDLL (at entry point
   enbufr_wrap.  */

enbufr_wrap(descr_,
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
            names_L,
            mesage_L);

return 0;
}

/* -------------------------------------------------- *
 *   IDES                                             *
 * -------------------------------------------------- */

#if defined F95
int ides_(Integer *fxxyyy_) {
#else
int IDES(Integer *fxxyyy_) {
#endif

int result;

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call IDES via C wrapper.  This loads BUFRDLL (at entry point
   ides_wrap.  */

result = ides_wrap(fxxyyy_);

return result;
}

/* -------------------------------------------------- *
 *   LOCALD                                           *
 * -------------------------------------------------- */

#if defined F95
int localdc_(Integer *x_,
#else
int LOCALDC(Integer *x_,
#endif
           Integer *y_,
           Integer *seq_,
           Integer *nseq_,
           char *text_,
           char *mode_,
           int text_L,
           int mode_L,
           int text_Len,
           int mode_Len) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call LOCALD via C wrapper.  This loads BUFRDLL (at entry point
   locald_wrap.  */

locald_wrap(x_,
            y_,
            seq_,
            nseq_,
            text_,
            mode_,
            text_L,
            mode_L);

return 0;
}

/* -------------------------------------------------- *
 *   ENBUFV4                                          *
 * -------------------------------------------------- */

#if defined F95
int enbuf4c_(Integer *descr_,
#else
int ENBUF4C(Integer *descr_,
#endif
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
            int     names_L,
            int     mesage_L,
            int     charsect1_L,
            int     charsect2_L,
            int     names_Len,
            int     mesage_Len,
            int     charsect1_Len,
            int     charsect2_Len) {

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call ENBUFV4 via C wrapper.  This loads BUFRDLL (at entry point
   enbufv4_wrap.  */

enbufv4_wrap(descr_,
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
             names_L,
             mesage_L,
             charsect1_L,
             charsect2_L);

return 0;
}

