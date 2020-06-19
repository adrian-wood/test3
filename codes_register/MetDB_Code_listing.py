import sys
import imp
import binascii
import datetime
from operator import itemgetter
import glob
import os.path
from os import path
import re
import string
import shlex
import subprocess
from subprocess import PIPE, Popen
import collections
import jinja2
from jinja2 import FileSystemLoader, Environment
from jinja2 import Template
#imp.reload(sys)
# sys.setdefaultencoding('utf-8')
loader = jinja2.FileSystemLoader(
    searchpath="/var/moods/code_register/template_ref/")
env = jinja2.Environment(loader=loader)

#
##############################################################################
# def: set_d
#
# purpose:  Establish default directories for the *.F90
# source code  & mod files. Take a listing of all
# *.F90, *.f90, *.c & *_mod files
#
# Input  : dir1 location and name of the MDB-repo established and passed by
# the shell script MetDB_cl.sh.
#
# Output :
# lbdir  (List) = listing of *.F90, *.f90 files
# lbdir2 (List) = listing of *.c files
# lbdir3 (List) = listing *_mod files
# d1 = source code directory for *.F90, *.f90 and *.C files
# d2 = source code directory for *_mod
#
##############################################################################


def set_d(dir1):
    d1list = []
    d2list = []
    # change or add new param
    d1 = dir1+'/MOODS/source/'
    d2 = dir1+'/MOODS/source/mods/'
    ############################
    d1.strip()
    d2.strip()
    d1list = os.listdir(d1)
    d2list = os.listdir(d2)
    list1 = [x for x in d1list if x.find(".F90") != -1 and
             x.find("_mod") == -1 and x.find("_MOD") == -1]
    list2 = [x for x in d1list if x.find(".f90") != -1 and
             x.find("_mod") == -1 and x.find("_MOD") == -1]
    list3 = [x for x in d1list if x.find(".c") != -1]
    list4 = [x for x in d2list if x.find("_mod") != -1]
    for xg in range(0, len(list4)):
        list4[xg] = d2 + list4[xg]
    for xh in range(0, len(list2)):
        list2[xh] = d1 + list2[xh]
    for xp in range(0, len(list3)):
        list3[xp] = d1 + list3[xp]
    for xf in range(0, len(list1)):
        list1[xf] = d1 + list1[xf]
    lbdir1 = list1 + list2
    lbdir2 = list3
    lbdir3 = list4
    return(lbdir1, lbdir2, lbdir3, d1, d2)
#
##############################################################################
#
# list_files
#
# Purpose:  Make listing of the files in the repository catagorised into
#           five groups, standard *.f90, interface, module,functions, and
#           *.c files.
#
# input:    lbf => listing of filenames (list)
#           switch => Identifier for type of filelisting
#                     1 = *.f90 & *.f90 functions
#                     2 = *_mod.f90
#                     3 = *.c
#
# output:   dictionary {'Codename (minus ext)': [[filename],[filelisting]]}
#                      ms1dict = *.f90
#                      ms2dict = *_mod.f90 (interface files)
#                      ms3dict = *_mod.f90 (module files)
#                      ms4dict = *.f90 (function files)
#                      ms5dict =  *.c
#
# NOTE:     Where switch = 2 the files will undergo a pattern search to
#           establish whether they are =>
#           1. Interface files (No internal functions and or subroutines)
#           2. Module Files (Internal functions and / or subroutines)
#
##############################################################################


def list_files(lbf, switch):
    # *.f90
    ms1dict = {}
    # interface
    ms2dict = {}
    # module
    ms3dict = {}
    # function
    ms4dict = {}
    # c files
    ms5dict = {}
    str1 = 'SUBROUTINE'
    str2 = 'FUNCTION'
    str3 = 'INTERFACE'
    str4 = 'MODULE'
    str5 = 'END'
    str6 = 'end'
    #
    for xg in range(0, len(lbf)):
        mname = lbf[xg].split('/')
        mfname = mname[-1:][0].strip('\n')
        if switch == 1 or switch == 2:
            mbfname = mfname[:-4]
        else:
            mbfname = mfname[:-2]
        #
        mbfname.replace(" ", "")
        #
        if switch == 1:
            dlistm = []
            dlistn = []
            dlistl = []
            dlistlu = []
            with open(lbf[xg], 'r') as fx:
                for linex in fx:
                    linex2 = linex.strip('\n')
                    linex2u = linex2.upper()
                    dlistl.append(linex2)
                    dlistlu.append(linex2u)
                fx.close()
                #
                indx1 = [i for i, p in enumerate(dlistlu) if
                         re.search(r"\b" + str1 + r"\b", p)]
                indx2 = [i for i, p in enumerate(dlistlu) if
                         re.search(r"\b" + str2 + r"\b", p)]
                indx3 = [i for i, p in enumerate(dlistlu) if
                         re.search(r"\b" + str3 + r"\b", p)]
                indx4 = [i for i, p in enumerate(dlistlu) if
                         re.search(r"\b" + str4 + r"\b", p)]
                #
                if len(indx1) == 0 and len(indx2) >= 1 and \
                   len(indx3) == 0 and len(indx4) == 0:
                    if indx2[0] < 5:
                        dlistmt = []
                        dlistmt2 = []
                        dlistmt2.append(mfname)
                        dlistmt.append(dlistmt2)
                        dlistmt.append(dlistl)
                        ms5dict[mbfname] = dlistmt
                else:
                    dlistmt = []
                    dlistmt2 = []
                    dlistmt2.append(mfname)
                    dlistmt.append(dlistmt2)
                    dlistmt.append(dlistl)
                    ms1dict[mbfname] = dlistmt
#
        elif switch == 2:       # process *_mod.f90 files into 2 groups
            dlistm2 = []
            dlistn2 = []
            dlistl2 = []
            dlistf = []
            dlistm3 = []
            dlistn3 = []
            dlistl3 = []
            dlistm4 = []
            dlistn4 = []
            dlistl4 = []
#
            with open(lbf[xg], 'r') as fz:
                for linez in fz:
                    linez2 = linez.strip('\n')
                    linez3 = linez2.strip()
                    dlistf.append(linez3)
                    if linez3.find('!') > -1:
                        p1 = linez3.find('!')
                        linez3t = linez3[:p1]
                        linez3 = linez3t
                    elif linez3.find('(') > -1:
                        q1 = linez3.find('(')
                        linez3q = linez3[:q1]
                        linez3 = linez3q
                    if linez3.find('::') == -1 \
                            and linez3.find('WRITE') == -1 \
                            and linez3.find('write') == -1 \
                            and linez3.find('print') == -1:
                        if not re.search(r"\b" + str5 + r"\b", linez3) \
                                and not re.search(r"\b" + str6 + r"\b", linez3):
                            linezu3 = linez3.upper()
                            dlistl2.append(linezu3)
                fz.close()
#
                indz1 = [i for i, p in enumerate(dlistl2)
                         if re.search(r"\b" + str1 + r"\b", p)]
                indz2 = [i for i, p in enumerate(dlistl2)
                         if re.search(r"\b" + str2 + r"\b", p)]
                indz3 = [i for i, p in enumerate(dlistl2)
                         if re.search(r"\b" + str3 + r"\b", p)]
                indz4 = [i for i, p in enumerate(dlistl2)
                         if re.search(r"\b" + str4 + r"\b", p)]
                if len(indz1) == 1 \
                        and len(indz2) == 0 \
                        and len(indz3) == 1 \
                        and len(indz4) == 1:
                    dlistn2.append(mfname)
                    dlistm2.append(dlistn2)
                    dlistm2.append(dlistf)
                    ms2dict[mbfname] = dlistm2
                elif len(indz1) == 0 \
                        and len(indz2) == 0 \
                        and len(indz3) == 0 \
                        and len(indz4) >= 1:           # alternative interface
                    dlistn2.append(mfname)
                    dlistm2.append(dlistn2)
                    dlistm2.append(dlistf)
                    ms2dict[mbfname] = dlistm2
                elif (len(indz1) + len(indz2)) >= 2 \
                        and len(indz3) == 0 \
                        and len(indz4) >= 1:           # indicates module
                    dlistn3.append(mfname)
                    dlistm3.append(dlistn3)
                    dlistm3.append(dlistf)
                    ms3dict[mbfname] = dlistm3
                elif len(indz1) == 0 \
                        and len(indz2) == 1 \
                        and len(indz3) == 1 \
                        and len(indz4) == 1:           # indicates function
                    dlistn2.append(mfname)
                    dlistm2.append(dlistn2)
                    dlistm2.append(dlistf)
                    ms2dict[mbfname] = dlistm2
        elif switch == 3:                              # *.c files
            dlistm3 = []
            dlistn3 = []
            dlistl3 = []
            with open(lbf[xg], 'r') as fc:
                for linec in fc:
                    linec2 = linec.strip('\n')
                    dlistl3.append(linec2)
                fc.close()
                dlistn3.append(mfname)
                dlistm3.append(dlistn3)
                dlistm3.append(dlistl3)
                mbfname2 = mbfname.strip()
                mbfname3 = mbfname2.lower()
                ms4dict[mbfname3] = dlistm3
    if switch == 1:         # *.f90 and *.f90 functions
        return(ms1dict, ms5dict)
    elif switch == 2:                 # *_mod  interface and module
        return(ms2dict, ms3dict)
    elif switch == 3:                 # *.c
        return(ms4dict)
#
# NOTE START TO REIDENT / TEST FROM HERE
# note dicts returned will contain [[code name ],[file listing]
#
##############################################################################
#
#  Purpose : Establish the useage of the file.
#
#  input    ddx1 => *.f90 dict of file listings
#           dd3 => *._mod.f90 (module) dict of file listings
#           dd4 => *.c  file listing
#           dd5 => *._mod.f90 (function) dict of file listings
#           dlx => directory required for def: add_link (called through
#                  def: com_dict
#
# Note: the useage of each file is made through a comparison with the files
# called in the dd1 (*.f90) dictionary listing only
#
#  output   The above dictionarys will be restructured with the
#           addition of an extra field (list) to reflect the
#           useage of the file (as below).
#
#           odx[key][0] = ['full code name']
#           odx[key][1] = ['code listing']
#           odx[key][2] = ['list of Mod interface files']
#           odx[key][3] = ['list of Internal Subroutines']
#           odx[key][4] = ['List of Internal Functions']
#           odx[key][5] = ['list of calls to external Subroutines' ]
#           odx[key][6] = ['Module - Subroutines calls ']
#           odx[key][7] = ['Module - Function calls']
#           odx[key][8] = ['*.c subroutine / function calls ']
#           odx[key][9] = ['*.F90 function calls ']
#           odx[key][10] = ['Called by ']
#
##############################################################################


def callby(ddx1, dd3, dd4, dd5, dlx):
    opd67 = {}
    #
    for keys3 in list(dd3.keys()):
        len1 = len(dd3[keys3][6])
        len2 = len(dd3[keys3][7])
#
    (opd5, opd6, opd7, opd8, opd9) = com_dict(ddx1, dd3, dd4, dd5, dlx)
    #
    for key7 in list(opd7.keys()):
        tlist7 = opd7[key7]
        if key7 in opd6:
            tlistf = tlist7 + opd6[7]
        else:
            tlistf = tlist7
        opd67[key7] = tlistf
    for key6 in list(opd6.keys()):
        if key6 not in opd67:
            opd67 = opd6[key6]
    #
    return(opd5, opd67, opd8, opd9)
#
#
##############################################################################
#
# com_dict
#           ddz1 - ddz5
#           ddzn => test dict referenced against (*.f90 dict)
#           dlq  => output directory required for def: add_link
#
##############################################################################


def com_dict(ddz1, ddz3, ddz4, ddz5, dlq):
    ddzn5 = {}
    ddzn6 = {}
    ddzn7 = {}
    ddzn8 = {}
    ddzn9 = {}
    ddzn51 = {}
    ddzn61 = {}
    ddzn71 = {}
    ddzn81 = {}
    ddzn91 = {}
#
    for keyx1 in list(ddz1.keys()):
        tlist5 = ddz1[keyx1][5]
        tlist6 = ddz1[keyx1][6]
        tlist7 = ddz1[keyx1][7]
        tlist8 = ddz1[keyx1][8]
        tlist9 = ddz1[keyx1][9]
        kopt = ddz1[keyx1][0][0]
#
        for xv1 in range(0, len(tlist5)):
            (sx1, sx2, sx3) = spstring(tlist5[xv1])
            if sx2 in list(ddzn5.keys()):
                nlist = ddzn5[sx2]
                nlist.append(kopt)
                ddzn5[sx2] = nlist
            else:
                nlist = []
                nlist.append(kopt)
                ddzn5[sx2] = nlist
#
        for xv6 in range(0, len(tlist6)):
            (sx1, sx2, sx3, sx4) = spstring(tlist6[xv1])
            if sx2 in list(ddzn6.keys()):
                nlist = ddzn6[sx2]
                nlist.append(kopt)
                ddzn6[sx2] = nlist
            else:
                nlist = []
                nlist.append(kopt)
                ddzn6[sx2] = nlist
#
        for xv7 in range(0, len(tlist7)):
            (sx1, sx2, sx3, sx4) = spstring(tlist7[xv7])
            if sx4 in list(ddzn7.keys()):
                nlist = ddzn7[sx4]
                nent = kopt + '-' + sx4 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn7[sx4] = nlist
            else:
                nlist = []
                nent = kopt + '-' + sx4 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn7[sx4] = nlist
#
        for xv8 in range(0, len(tlist8)):
            (sx1, sx2, sx3, sx4) = spstring(tlist8[xv8])
            if sx4 in list(ddzn8.keys()):
                nlist = ddzn8[sx4]
                nent = kopt + '-' + sx4 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn8[sx4] = nlist
            else:
                nlist = []
                nent = kopt + '-' + sx4 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn8[sx4] = nlist
#
        for xv9 in range(0, len(tlist9)):
            (sx1, sx2, sx3) = spstring(tlist9[xv9])
            if sx2 in list(ddzn9.keys()):
                nlist = ddzn9[sx2]
                nent = kopt + '-' + sx1 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn9[sx2] = nlist
            else:
                nlist = []
                nent = kopt + '-' + sx1 + '(' + sx2 + ')'
                nlist.append(nent)
                ddzn9[sx2] = nlist
#
    for key1z in list(ddzn5.keys()):
        tlist = ddzn5[key1z]
        tlistf2 = []
        tlistf = list(dict.fromkeys(tlist))
        for zx in range(0, len(tlistf)):
            tbl = ''
            spx2sp = tlistf[zx].split('.')
            spx2 = spx2sp[0]
            sx2 = tlistf[zx]
            sx4 = sx2
            sx3 = ''
            tbl = sx2
            sw = 1
            axd = clinkn(ddz1, spx2, sx3, dlq, sw)
            tlistf2.append(axd)
        ddzn51[key1z] = tlistf2
#
    for key2z in list(ddzn6.keys()):
        tlist = ddzn6[key2z]
        tlistf = list(dict.fromkeys(tlist))
        ddzn61[key2z] = tlistf
#
    for key3z in list(ddzn7.keys()):
        tlist = ddzn7[key3z]
        tlistf62 = []
        tlistf6 = list(dict.fromkeys(tlist))
        for xfz in range(0, len(tlistf6)):
            (fsp1, fsp2, fsp3, fsp4) = spstring(tlistf6[xfz])
            tbl6 = tlistf6[xfz]
            sx3 = ''
            sw = 4
            spx3sp = tlistf6[xfz].split('.')
            spx3 = spx3sp[0]
            axd2a = clinkn(ddz1, spx3, tlistf6[xfz], dlq, sw)
            tlistf62.append(axd2a)
        ddzn71[key3z] = tlistf62
#
    for key4z in list(ddzn8.keys()):
        tlist8 = ddzn8[key4z]
        tlistf82 = []
        tlistf8 = list(dict.fromkeys(tlist8))
        for xfy in range(0, len(tlistf8)):
            (fsx1, fsx2, fsx3, fsx4) = spstring(tlistf8[xfy])
            tbl8 = tlistf8[xfy]
            sw8 = 4
            spx8sp = tlistf8[xfy].split('.')
            spx8 = spx8sp[0]
            axd8a = clinkn(ddz1, spx8, tlistf8[xfy], dlq, sw8)
            tlistf82.append(axd8a)
        ddzn81[key4z] = sorted(tlistf82)
#
    for key5z in list(ddzn9.keys()):
        tlist = ddzn9[key5z]
        tlistf92 = []
        tlistf9 = list(dict.fromkeys(tlist))
        for xfq in range(0, len(tlistf9)):
            (fsp91, fsp92, fsp93, fsp94) = spstring(tlistf9[xfq])
            tbl9 = tlistf9[xfq]
            sw9 = 4
            spx9sp = tlistf9[xfq].split('.')
            spx9 = spx8sp[0]
            axd9a = clinkn(ddz1, spx9, tlistf9[xfq], dlq, sw9)
            tlistf92.append(axd9a)
        ddzn91[key5z] = tlistf92
#
    return(ddzn51, ddzn61, ddzn71, ddzn81, ddzn91)
#
##############################################################################
#
# tree_dr : Main subroutine for the processing of code listings.
#
# input :
#           inflist => *.f90 dict of file listings
#           pbli => *._mod.f90 (interface) dict of file listings
#           pblm => *._mod.f90 (module) dict of file listings
#           pblf => *._mod.f90 (function) dict of file listings
#           pblc => *.c  file listing
#
#           calls =>
#           treedr :  Using the lists as designated above as
#                     input and a switch (int - sw) will return
#                     a list of line numbers where the search
#                     string has been found.
#
#           sum_srch : Using the dict output from treedr for the
#                      *.f90 files (od1) and the *.c files (od4)
#                      , search each *.f90 listing for calls / references to
#                      c subroutines / functions.
#
# output:   5 dictionaries (of lists) containing details of internal and
#           external dependencies.
#
#           od1 :   *.f90 and *.F90 files
#           od2 :   *_mod.f90 and *_mod.F90 files (modules)
#           od3 :   *_mod.f90 and *_mod.F90 files (interface)
#           od4 :   *.c files
#           od5 :    *.f90 and  *.F90 files (functions)
#
#           each of the output dictionaries formatted as below
#           odx[key][0] = ['full code name']
#           odx[key][1] = ['code listing']
#           odx[key][2] = ['list of Mod interface files']
#           odx[key][3] = ['list of Internal Subroutines']
#           odx[key][4] = ['List of Internal Functions']
#           odx[key][5] = ['list of calls to external Subroutines' ]
#           odx[key][6] = ['Module - Subroutines calls ']
#           odx[key][7] = ['Module - Function calls']
#           odx[key][8] = ['*.c subroutine / function calls ']
#           odx[key][9] = ['*.F90 function calls ']
#           odx[key][10] = ['Called by ']
#
#
##############################################################################


def tree_dr(inflist, pbli, pblm, pblc, pblf):
    linkd = {}
    list1 = []
    lista = []
    nmod = []
    ftype = ['int ', 'float ', 'double ', 'void ',
             'extern ', 'int64', 'int64_t']
#
    od1 = {}       # module files
    od2 = {}       # mod interface files
    od3 = {}       # calls to *.f90 etc
    od4 = {}       # *.c
    od5 = {}       # *.f90 functions
#
#  test list for comparison
    tlist = []
    tlistc = []
    for keyt in list(inflist.keys()):
        tlist.append(keyt)  # temp change
#
##############################################################################
#
    sw = 2
    od2 = treedr(pbli, sw, tlist)          # pbli = mod.f90
#
    sw = 3
    od3 = treedr(pblm, sw, tlist)          # pblm = module filenames
#
    sw = 5
    od5 = treedr(pblf, sw, tlist)          # pblf = function filenames
#
    sw = 1
    od1 = treedr(inflist, sw, tlist)       # inflist = *.f90 filenames
#
    sw = 4
    od4 = treedr(pblc, sw, tlist)          # pblc = *c filenames
#
    (od1a, od2a) = sum_srch(od1, od2, od3, od4, od5)
    od1 = od1a
    od2 = od2a
    sfh = sdict(od1, od3)
    od1 = sfh
#
    clist4 = []
    clist4f = []
#
    for keyc in list(od4.keys()):
        clist4f = clist4 + od4[keyc][3]
        clist4 = clist4f
        clist4f = []
#
    for key1 in list(od1.keys()):
        nlist = []
        tlist = od1[key1][5]
        clist = od1[key1][8]
        for x1 in range(0, len(tlist)):
            (xc1, xc2, xc3) = spstring(tlist[x1])
            nc = -1
            txc2 = '(' + xc2 + ')'
            for zx2 in range(0, len(clist4)):
                if clist4[zx2].find(txc2) > -1:
                    nc = 1
            if nc == -1:
                nlist.append(tlist[x1])
        od1[key1][5] = nlist
#
    return(od1, od2, od3, od4, od5)
#
#
##############################################################################
#
#  Name:  treedr:
#
#  Purpose: Create and populate lists of dependencies as set out below -
#
#
#           [0]['full code name']
#           [1]['code listing']
#           [2]['list of Mod interace']
#           [3]['list of Internal Subroutines']
#           [4]['List of Internal Functions']
#           [5]['list of calls to external Subroutines' ]
#           [6]['List of Module files']
#           [7]['*.c subroutine / function calls ']
#
#  Note in the output the fields [6] & [7] will not be populated. Calls to
#  Module & *.c files will be contained in [5]. This is due to the intial
#  search which would have used 'call' to identify the sub-string
#
#  called by : tree_dr
#
#  input : indict   =>   Input dictionary as defined in tree_dr
#
#          switchcp =>   int to signify the type of dict list
#                        to be processed , designated as below
#                        switchcp == 1  *.f90 listing of filenames
#                        switchcp == 2  *._mod.f90 (interface) dict
#                                       of file listings
#                        switchcp == 3  *._mod.f90 (module) dict of
#                                       file listings
#                        switchcp == 4  *.c  file
#                        switchcp == 5  *.f90 (function) dict of
#                                        file listings
#
#
#  output : dict of lists as specified above
#
#  calls :  find_sf
#
#
#
##############################################################################


def treedr(indict, switchp, tlq):
    outdict1 = {}
    outdict2 = {}
    outdict3 = {}
    outdict4 = {}
    outdict5 = {}
    mdp1 = []
    mdp2 = []
    mdp3 = []
    mdp4 = []
    mdpb = []
    moff = 0
#
#
    ftype = ['int ', 'float ', 'double ', 'void ',
             'extern ', 'int64', 'int64_t']
    for keyp in list(indict.keys()):
        mdp1 = []
        mdp2 = []
        mdp3 = []
        mdp4 = []
        mdpb = []
        pblt1 = []
        pblt2 = []
        mdp1 = []
        inplist = indict[keyp][1]
        for xpz in range(0, len(inplist)):
            if inplist[xpz].find('!') > -1:
                pnum = inplist[xpz].find('!')
                pblt1.append(inplist[xpz][:pnum])
            else:
                pblt1.append(inplist[xpz])
        pblt2 = [x.upper() for x in pblt1]
        if switchp == 1 or switchp == 2 or switchp == 3 or switchp == 5:
            (ind1r, ind2r, ind3r, ind4r) = search_sub(switchp, pblt2, keyp)
            if switchp != 4:
                dPstring = ['USE', 'MOD']
                mdp1 = find_sf(ind1r, dPstring, keyp, inplist)
            if switchp != 4:
                dPstring = ['SUBROUTINE']
                mdp2 = find_sf(ind2r, dPstring, keyp, inplist)
            if switchp != 4:
                dPstring = ['FUNCTION']
                mdp3 = find_sf(ind3r, dPstring, keyp, inplist)
            if switchp != 4:
                dPstring = ['CALL']
                mdp4 = find_sf(ind4r, dPstring, keyp, inplist)
#
        elif switchp == 4:
            fdl = []
            ind4r = search_sub(switchp, pblt2, keyp)
            for xtr in range(0, len(ind4r)):
                linexc = inplist[ind4r[xtr]]
                if linexc.find('/*') > -1:
                    moff = 1
                elif linexc.find('*/') > -1:
                    moff = 0
                if moff == 0:
                    for xts in range(0, len(ftype)):
                        sdf = linexc.find(ftype[xts])
                        sdf2 = linexc.find('(')
                        if sdf > -1 and sdf2 > -1 and sdf < sdf2:
                            adstr = linexc[sdf:sdf2]
                            pbx = ' '.join(adstr.split())
                            pbxf = pbx.split()
                            plhb = pbxf[-1:][0].lower()
                            if plhb not in tlq:
                                adstrf = keyp + '(' + pbxf[-1:][0] + ')'\
                                        + '-' + str(ind4r[xtr])
                                adstrf2 = adstrf.replace(" ", "")
                                if adstrf2 not in fdl:
                                    fdl.append(adstrf2)
            mdp2 = fdl

#
        fdict = indict[keyp]  # index || Description
        fdict.append(mdp1)    # 2 =>   '*_mod' files
        fdict.append(mdp2)    # 3 =>   'internal subroutines
        fdict.append(mdp3)    # 4 =>    internal functions
        fdict.append(mdp4)    # 5 =>    calls to other subroutines etc.
        fdict.append(mdpb)    # 6 =>    Module - Subroutines calls
        fdict.append(mdpb)    # 7 =>    Module - Function calls
        fdict.append(mdpb)    # 8 =>    *.c subroutine / function calls
        fdict.append(mdpb)    # 9 =>    *.F90 function calls
#
        ind1r = []
        ind2r = []
        ind3r = []
        ind4r = []
        ind5r = []
#
        if switchp == 1:
            keypl = keyp.lower()
            outdict1[keypl] = fdict
        elif switchp == 2:
            keypl = keyp.lower()
            outdict2[keypl] = fdict
        elif switchp == 3:
            keypl = keyp.lower()
            outdict3[keypl] = fdict
        elif switchp == 4:
            keypl = keyp.lower()
            outdict4[keypl] = fdict
        elif switchp == 5:
            keypl = keyp.lower()
            outdict5[keypl] = fdict
#
    if switchp == 1:                           # *.f90 filenames
        return(outdict1)
    elif switchp == 2:                         # *_mod files (interface)
        return(outdict2)
    elif switchp == 3:                         # *_mod files (module)
        return(outdict3)
    elif switchp == 4:                         # *.c filenames
        return(outdict4)
    elif switchp == 5:                         # *.f90 function filenames
        return(outdict5)
#
##############################################################################
#
#   Name: find_sf
#
#   Purpose: Search through file listing (iplr) indf and check the line no's
#            corresponding to those in indf (list) for occurences of the
#            strings given in dflist (list). These will be set to
#            'function' or  'subroutine or 'use' & 'mod'.
#            Search the lines identified and return the matching string as
#            follows -
#            program_name(calling func name) - line_no
#
#   Input -: indf     => list of line nos to check for string matches
#            dflist   => list ['function', subroutine', 'call'] etc
#            keysf    => key corresponding to the prog entry
#            iplr     => program listing =>  [Line1, line2, line3 ]
#
#   Output -: mdpt LIST of matching entries where each is formatted as :-
#             program_name(calling func name) - line_no
#
#  called by : treedr
#
##############################################################################


def find_sf(indf, dflist, keysf, iplr):
    mdpt = []
#
    sflen = len(dflist)
    pblt = [x.upper() for x in iplr]
    if len(dflist) > 1:
        dflist1 = dflist[0]
        dflist2 = dflist[1]
#
    else:
        dflist1 = dflist[0]
        dflist2 = 99
#
    for xd in range(0, len(indf)):
        idg = indf[xd]
        tqline = iplr[idg]
        dflen = len(dflist[0])
        mde1 = pblt[idg].find(dflist[0])
        if mde1 > -1:
            ent3 = tqline[mde1 + dflen:]
            ent3a = ' '.join(ent3.split())
            dfh = ent3a.find('(')
            dfh1 = ent3a.find('&')
            dfh2 = ent3a.find(',only:')
            if dfh > -1:
                tfh1 = ent3a[:dfh]
            elif dfh1 > -1:
                tfh1 = ent3a[:dfh1]
            else:
                tfh1p = ent3a.split()
                tfh1 = tfh1p[0]
            tfh1s = tfh1.strip()
            keysf2 = keysf.upper()
            if tfh1s != keysf2:
                if dfh > -1 or dfh1 > -1:
                    sbhname = keysf + '(' + ent3a[:dfh] + ')' + \
                                ' - ' + str(indf[xd])
                    sbhname2 = sbhname.replace(" ", "")
                else:
                    sbhname = keysf + '(' + ent3 + ')' + ' - ' \
                                + str(indf[xd])
                    sbhname2 = sbhname.replace(" ", "")
                if sbhname2 not in mdpt:
                    mdpt.append(sbhname2.lower())
    return(mdpt)
#
##############################################################################
#
#   Name:           sdict
#
#   Purpose:        run check against dict2 (module (subroutine and
#                   functions) for match.
#
#
##############################################################################


def sdict(dt1, dt2):
    #
    refd = {}
    for keyx in list(dt2.keys()):
        ref3 = dt2[keyx][3]
        ref4 = dt2[keyx][4]
        refn = ref3 + ref4
        refln = []
        for xpn in range(0, len(refn)):
            (rf1, rf2, rfn) = spstring(refn[xpn])
            if rf2 not in refln:
                refln.append(rf2)
        refd[keyx] = refln
    #
    for keyz in list(dt1.keys()):
        tlist6 = dt1[keyz][6]
        if len(tlist6) > 0:
            tfinal = []
            for xc in range(0, len(tlist6)):
                (x1, x2, xn) = spstring(tlist6[xc])
                tbr = 1
                for keytx in list(refd.keys()):
                    dxf = refd[keytx]
                    for xz in range(0, len(dxf)):
                        if dxf[xz] == x2:
                            ndc = x1 + '(' + keytx + '(' + x2 + \
                                  ')' + ')-' + str(xn)
                            tfinal.append(ndc)
                            tbr = -1
                            break
                if tbr == -1:
                    break
            dt1[keyz][6] = tfinal
#
    for keyz2 in list(dt1.keys()):
        tlist7 = dt1[keyz2][7]
        if len(tlist7) > 0:
            tfinal = []
            for xc2 in range(0, len(tlist7)):
                (x1, x2, xn) = spstring(tlist7[xc2])
                tbr2 = 1
                for keytx2 in list(refd.keys()):
                    dxf2 = refd[keytx2]
                    for xz2 in range(0, len(dxf2)):
                        if dxf2[xz2] == x2:
                            ndc2 = x1 + '(' + keytx2 + '(' + x2 + ')' \
                                    + ')-' + str(xn)
                            tfinal.append(ndc2)
                            tbr2 = -1
                            break
                    if tbr2 == -1:
                        break
            dt1[keyz2][7] = tfinal
    return(dt1)
#
##############################################################################
#
#  Name: search_sub
#
#  input :  pblu => code listing extracted from the dictionaries 1 => 5
#           element[1] as detailed in tree_dr.
#           switchx 1 => 4 *.f90 file
#           switchx   => 5 *.c file
#
#  output:  if switch == 1 - 4: ind1t, ind2t, ind3t, ind4t,
#           LIST OF line numbers where matches are found for
#           string searches on 'SUBROUTINE, USE & _MOD, FUNCTION & CALL
#
#           If switch == 5
#           LIST of line numbers where the search for the data types
#           specified in the list ftype have been found.
#
# called by:  treedr
#
##############################################################################


def search_sub(switchx, pblu, tkr):
    #
    indmt = []
    ind1t = []
    ind1ta = []
    ind2ta = []
    ind3ta = []
    ind4ta = []
    ind2t = []
    ind3t = []
    ind4t = []
    ind5t = []
    ftype = ['INT ', 'FLOAT ', 'DOUBLE ', 'VOID ', 'INT64', 'INT64_T']
    #
    if switchx == 1 or switchx == 2 or switchx == 3 or switchx == 5:
        for xpz in range(0, len(pblu)):
            lnq = pblu[xpz]
            if lnq.find('USE') > -1 and lnq.find('_MOD') > -1:
                if lnq.find('!') > -1 and lnq.find('::') == -1:
                    if lnq.find('!') > lnq.find('_MOD'):
                        ind1ta.append(xpz)
                elif lnq.find('!') == -1 and lnq.find('::') == -1:
                    ind1ta.append(xpz)
#
        for xpz in range(0, len(pblu)):
            lnq = pblu[xpz]
            if lnq.find('SUBROUTINE ') > -1 and \
                    lnq.find('END') == -1 and xpz > 0:
                if lnq.find('USE') == -1 and \
                        lnq.find('WRITE') == -1 and xpz > 0:
                    if lnq.find('!') == -1 and \
                            lnq.find('::') == -1:
                        ind2ta.append(xpz)
#
        for xpz in range(0, len(pblu)):
            lnq = pblu[xpz]
            if lnq.find('FUNCTION ') > -1 and \
                    lnq.find('END') == -1 and xpz > 0:
                if lnq.find('!') == -1 and \
                        lnq.find('::') == -1 and xpz > 0:
                    if lnq.find('USE') == -1 and\
                            lnq.find('WRITE') == -1:
                        ind3ta.append(xpz)
#
        for xpz in range(0, len(pblu)):
            lnq = pblu[xpz]
            if lnq.find('CALL ') > -1 and lnq.find('WRITE') == -1:
                tval = lnq.find('!')
                tval2 = lnq.find('CALL ')
                if tval > -1 and tval > tval2:
                    ind4ta.append(xpz)
                else:
                    ind4ta.append(xpz)
    if switchx == 4:
        for xtr in range(0, len(ftype)):
            ind1a = []
            ind1a = [i for i, p in enumerate(pblu) if (ftype[xtr] in p)
                     and ('(' in p) and ('//' not in p) and
                     ('*' not in p[0:5]) and ('/*' not in p)
                     and ('EXTERN' not in p)]
            if len(ind1a) > 0:
                ind5t.append(ind1a)
        f5ind = [item for sublist in ind5t for item in sublist]
        f51 = list(set(f5ind))
        f52 = sorted(f51, key=int)
        ind5t = f52
#
    if switchx == 1 or switchx == 2 or switchx == 3 or switchx == 5:
        return(ind1ta, ind2ta, ind3ta, ind4ta)
    if switchx == 4:
        return(ind5t)
#
##############################################################################
#
#  Name : sum_srch
#
#  Purpose:            Iterate through the *.f90 listings (idict) and cross
#                      reference against against xdict for references to
#                      f90 functions , subroutines  and / *.c calls given
#                      in xdict.
#
#  input: #
#
#  output:   idict  :  Revised version of the input directory idict
#                      will have fields (as below) completed :
#
#
##############################################################################


def sum_srch(xd1, xd2, xd3, xd4, xd5):
    clist = []
    clistf = []
    dlistf = []
    elistf = []
    dx2l = []
    ex2l = []
    flist = []
    flist2 = []
    isfln = []
    iffln = []
    nlist2 = []
    nmlist = []
    nm2list = []
    nm3list = []
    nm4list = []
    clistf1 = []
    clistf2 = []
    fclist = []
    fclist2 = []
    elist1 = []
    elist2 = []
    tlist2 = []
    tlist2f = []
    f9fnc = []
    f9fncf = []
    f9md = []
    f9mdf = []
#
###############################################
# 1 create summary of the module , function
#   and *.c code names output to temp list
#
###############################################
#
    # xd4 dictionary == *.c listings
    for keyx in list(xd4.keys()):
        clist = xd4[keyx][3]
        for xc in range(0, len(clist)):
            (cl1, cl2, cl3) = spstring(clist[xc])
            clf = cl1 + '(' + cl2 + ')'
            clistf1.append(clf.lower())
            clistf2.append(cl2.lower())
#
###############
#
    # xd3 ==  *.f90  module file
    for keyt in list(xd3.keys()):
        fclist = xd3[keyt][4]
#
        for xd in range(0, len(fclist)):
            (fl1, fl2, fl3) = spstring(fclist[xd])
            fclf = fl1 + '(' + fl2 + ')'
            fclist.append(fclf.lower())
            fclist2.append(fl2.lower())
#
        fblist = xd3[keyt][3]
        for xt in range(0, len(fblist)):

            (el1, el2, el3) = spstring(fblist[xt])
            el1f = el1 + '(' + el2 + ')'
            elist1.append(el1f.lower())
            elist2.append(el2.lower())
#
#######
#
    flist = []
    flist2 = []
    for keyp in list(xd5.keys()):
        # od5 ==  *.f90 function
        mname = keyp
        flt1 = xd5[keyp][0][0]
        fmname = mname + '(' + flt1 + ')'
        if fmname not in flist:
            flist.append(fmname)
        if mname not in flist2:
            flist2.append(mname)
#
##############################################################################
#
# 2. process the data
#
##############################################################################
#
#   Process data
#   i)   check current listings (from od1 (xd1)) for
#        *_mod.f90 module entries
#        listed in od1[key][5] and will be of the
#        form 'program_name(subroutine)-line_no'.
#        Cross reference the subroutine name against
#        the od2 internal subroutine
#        and function listing given in dlistf and elistf
#        ( Module  Subroutines
#        and function names) If a match is found replace
#        the od1[key][5] entry with
#        'module_name(subroutine)-line_no'.
#
#   ii)  check current listings for *.c references
#        'Call's' made within the code may reference
#         *.c subroutines. where this
#         is the case the [5] entry may be incorrect.
#         Change this to
#         c code_name(subroutine)-line_no
#
#   iii) check current listings for *_mod functions and search for key words
#        The intial search for 'call' may not have identified the entries
#        within the listing for functions such as 'IVALUE'.
#        The following steps will be taken
#        1. Establish from the [2] list which are actual *.f90 functions
#           and compile a sub list. Search the code listing for these entries
#           and make an entry in od1[6] of the form
#          'function_name(subroutine)-line_no'.
#
##############################################################################
    for keyx in list(xd1.keys()):
        xlist = []
        z8list = []
        x2list = []
        z6list = []
        x3list = []
        z7list = []
        plist = []
        tlist = []
        clist = []
        zlistq = []
        fclist = []
        fnlist = []
        fsub = xd1[keyx][1]          # full code listing
        imod = xd1[keyx][2]	     # mod files
        imod2 = xd1[keyx][5]         # calls to subroutines
        imod2l = imod2
        fsub2 = fsub
#
        for xz in range(0, len(imod2l)):
            (xz1, xz2, xz3) = spstring(imod2l[xz])
            for xc in range(0, len(clistf2)):
                if clistf2[xc] == xz2:
                    tfile = xz1 + '(' + clistf1[xc] + ')-' + xz3
                    z8list.append(tfile)
#
        for xy in range(0, len(imod2l)):
            (xy1, xy2, xy3) = spstring(imod2l[xy])
            for fx in range(0, len(fclist2)):
                if fclist2[fx] == xy2:
                    z6list.append(imod2l[xy])
#
        for xa in range(0, len(imod2l)):
            (xz1, xz2, xz3) = spstring(imod2l[xa])
            for fz in range(0, len(elist2)):
                if elist2[fz] == xz2:
                    z7list.append(imod2l[xa])
        dlist = set(imod2l) - set(z8list + z7list + z6list)
        dlist2 = list(dlist)
#
        slist = []
        for xp in range(0, len(imod)):
            (xv1, xv2, xv3) = spstring(imod[xp])
            xv2f = xv2[:-4]
            res1 = [i for i in flist if xv2 in i]
            res2 = [i for i in elist1 if xv2 in i]
            if xv2f in flist2:
                slist.append(xv2f)
#
        for zx in range(0, len(slist)):
            xitem = slist[zx]
            nxitem = len(xitem)
            for xd in range(0, len(fsub2)):
                fpin = fsub2[xd].lower()
                if fpin.find(xitem) > -1:
                    if fpin.find('=') > -1 and fpin.find('(') > -1:
                        fx1 = fpin.replace('=', ' ')
                        fx2 = fx1.replace('(', ' ')
                        ' '.join(fx2.split())
                        fpin = fx2
                        fpind = fpin.split()
                        if xitem in fpind:
                            if xitem in list(xd1.keys()):
                                xitem2 = xd1[xitem][0][0]
                            else:
                                xitem2 = xitem + '_mod'
                            fent = xitem2 + '(' + xitem + ')' + '-' + str(xd)
                            fnlist.append(fent)
#
        xd1[keyx][5] = dlist2
        xd1[keyx][6] = z6list
        xd1[keyx][7] = z7list
        xd1[keyx][8] = z8list
        xd1[keyx][9] = fnlist
#
    return(xd1, xd2)
#
##############################################################################
#
#  Name     :   recon
#
#  Purpose  :   From the dictionary code list entries for tdq (as below)
#               locate source code references and add html link
#
#  Input    :   xlink - output directory path supplied through the shell script
#               tq1 => Dictionary of lists for the *.f90 code
#               tq2 => Dictionary of lists for the  module  code
#               tq3 => Dictionary of lists for the *_mod.f90 interface code
#               tq4 => Dictionary of lists for the *.c code
#               tq5 => Dictionary of lists for the *_mod.f90 functions
#               df  => switch to determine to which Dictionary
#                      changes will be applied
#
#               Each i/p dictionary will be of the form :
#
#               odx[key][0] = ['full code name']
#               odx[key][1] = ['code listing']
#               odx[key][2] = ['list of Mod interface files']
#               odx[key][3] = ['list of Internal Subroutines']
#               odx[key][4] = ['List of Internal Functions']
#               odx[key][5] = ['list of calls to external Subroutines' ]
#               odx[key][6] = ['Module - Subroutines calls ']
#               odx[key][7] = ['Module - Function calls']
#               odx[key][8] = ['*.c subroutine / function calls ']
#               odx[key][9] = ['*.F90 function calls ']
#               odx[key][10] = ['Called by ']
#
#  Output   :
#
##############################################################################


def recon(xlink, tq1, tq2, tq3, tq4, tq5, df):
    fpdict = {}
    if df == 1:
        tqn = tq1
    elif df == 2:
        tqn = tq2
    elif df == 3:
        tqn = tq3
    elif df == 4:
        tqn = tq4
    elif df == 5:
        tqn = tq5
    #
    for keyxp in list(tqn.keys()):
        tlist = tqn[keyxp]
        opname = tlist[0][0]
        flist = tlist[1]    # code listing
        n2list = tlist[2]   # 'mod' interface files
        n3list = tlist[3]   # 'list of Internal Subroutines'
        n4list = tlist[4]   # 'List of Internal Functions'
        n5list = tlist[5]   # 'list of calls to external Subroutines'
        n6list = tlist[6]   # 'Module - Subroutines calls '
        n7list = tlist[7]   # 'Module - Function calls'
        n8list = tlist[8]   # '*.c subroutine / function calls '
        n9list = tlist[9]   # '*.F90 function calls '
    #
        for x2 in range(0, len(n2list)):
            (sv1, sv2, sv3) = spstring(n2list[x2])
            sv2f = sv2.strip()
            if sv2f in list(tq2.keys()):
                tline2 = flist[int(sv3)]
                tline2l = tline2.lower()
                sw = 4
                sfx = ''
                tlinef2a = clinkn(tq2, sv2, sv2, xlink, sw)
                tlinef2c = tline2l.replace(sv2, tlinef2a)
                flist[int(sv3)] = tlinef2c
            elif sv2f in list(tq5.keys()):
                tline2 = flist[int(sv3)]
                tline2l = tline2.lower()
                sw = 4
                sfx = ''
                tlinef2a = clinkn(tq5, sv2, sv2, xlink, sw)
                tlinef2c = tline2l.replace(sv2, tlinef2a)
                flist[int(sv3)] = tlinef2c
            elif sv2f in list(tq3.keys()):
                tline2 = flist[int(sv3)]
                tline2l = tline2.lower()
                sw = 4
                sfx = ''
                tlinef3a = clinkn(tq3, sv2, sv2, xlink, sw)
                tlinef3c = tline2l.replace(sv2, tlinef3a)
                flist[int(sv3)] = tlinef3c
        for x5 in range(0, len(n5list)):
            (sv1, sv2, sv3) = spstring(n5list[x5])
            if sv2 in list(tq1.keys()):
                tline5 = flist[int(sv3)]
                tline5l = tline5.lower()
                sw = 4
                tlinef5a = clinkn(tq1, sv2, sv2, xlink, sw)
                tline5d = tline5l.replace(sv2, tlinef5a)
                flist[int(sv3)] = tline5d
        for x6 in range(0, len(n6list)):
            (sv1, sv2, sv3) = spstring(n6list[x6])
            if sv2 in list(tq3.keys()):
                tline6 = flist[int(sv3)]
                tline6l = tline6.lower()
                sw = 4
                tlinef6a = clinkn(tq1, sv2, sv2, xlink, sw)
                tline6d = tline6l.replace(sv2, tlinef6a)
                flist[int(sv3)] = tlinef6d
        for x7 in range(0, len(n7list)):
            (sv1, sv2, sv3, sv4) = spstring(n7list[x7])
            if sv4 in list(tq3.keys()):
                tline7 = flist[int(sv3)]
                tline7l = tline7.lower()
                sw = 4
                tlinef7a = clinkn(tq3, sv4, sv2, xlink, sw)
                tline7d = tline7l.replace(sv2, tlinef7a)
                flist[int(sv3)] = tline7d
#
        for x8 in range(0, len(n8list)):
            (sv1, sv2, sv3, sv4) = spstring(n8list[x8])
            if sv4 in list(tq4.keys()):
                tline8 = flist[int(sv3)]
                tline8l = tline8.lower()
                sw = 4
                tlinef8a = clinkn(tq4, sv4, sv2, xlink, sw)
                tline8d = tline8l.replace(sv2, tlinef8a)
                flist[int(sv3)] = tline8d
#
        for x9 in range(0, len(n9list)):
            (sv1, sv2, sv3) = spstring(n9list[x9])
            if sv2 in list(tq1.keys()) or sv2 in list(tq5.keys()):
                if sv2 in list(tq1.keys()):
                    tqx = tq1
                else:
                    tqx = tq5
                tline9 = flist[int(sv3)]
                tline9l = tline9.lower()
                sw = 4
                tlinef9a = clinkn(tqx, sv2, sv2, xlink, sw)
                tline9d = tline9l.replace(sv2, tlinef9a)
                flist[int(sv3)] = tline9d
#
        fpdict[keyxp] = flist
#
        tqn[keyxp][1] = flist
    return(tqn)
#
##############################################################################
#
#   Name     : clinkn
#
#   Purpose  : construct link from parameters supplied
#
#   Input    : dqx   => dictionary / list to cross reference against
#              sfn   => calling filename (minus file ext)
#              sfx   => module name (if different from filename)
#              xlink => directory path supplied through shell script
#              sw    => switch to determine the structure of the output
#                       & link
#   1 = <a href=\'filename'_code_listing.html\> filename<\a>
#   2 = <a href=\'filename'_code_listing.html\> filename<\a>
#   3 = <a href=\'filename'_code_listing.html\> filename(module name)<\a>
#   4 = <a href=\'filename'_code_listing.html\> module_name<\a>
#
#   Output   : html link
#
##############################################################################


def clinkn(dqx, sfn, sfx, xlink, sw):
    lnkmn = ''
    if sw == 1:
        if sfn in list(dqx.keys()):
            fmn = dqx[sfn][0][0]
            lnkmn = '<a href=\"' + xlink + sfn + '_code_listing.html\" >'\
                    + fmn + '</a>'
    elif sw == 2:
        if sfn in list(dqx.keys()):
            lnkmn = '<a href=\"' + xlink + sfn + '_code_listing.html\" >'\
                    + sfn + '</a>'
    elif sw == 3:
        if sfn in list(dqx.keys()):
            sfx2 = sfn + '(' + sfx + ')'
            lnkmn = '<a href=\"' + xlink + sfn + '_code_listing.html\" >'\
                    + sfx2 + '</a>'
    elif sw == 4:
        if sfn in list(dqx.keys()):
            lnkmn = '<a href=\"' + xlink + sfn + '_code_listing.html\" >'\
                     + sfx + '</a>'
    elif sw == 5:
        if sfn in list(dqx.keys()):
            lnkmn = '<a href=\"' + xlink + sfn + '_Cover.html\" >' + \
                     sfx + '</a>'
    elif sw == 99:
        lnkmn = '<a href=\"' + xlink + sfn + '_depend.html\" >' + sfn + \
                '_depend.html' + '</a>'
#
    return(lnkmn)
#
##############################################################################
#
#
##############################################################################


def ftl_out(dinp, dlk, dlk2):
    topl = 'template_listing_master.html'
    topl2 = env.get_template(topl)
    for keyzl in list(dinp.keys()):
        fghil = keyzl
        dlp = dinp[keyzl][1]
        oplm = dlk2 + fghil + '_code_listing.html'
        outputm = topl2.render(fname=fghil, clist=dlp)
        with open(oplm, 'w') as flm:
            flm.write(outputm)
        flm.close()
#
##############################################################################
#
#   Name     : spstring
#
#   Purpose  : Split string formatted as 'name(sub_name)-line_no'
#              staray(metdb_com_mod,only:MISSIN,RMISS)-88
#              into
#              var1 = name
#              var2 = sub_name
#              var3 = line no
#
#   Input    : sdf - String formatted as above
#
#   Output   : sdfp  = name
#              sdfp2 = sub_name
#              sdfn  = line no
#
##############################################################################


def spstring(sdf):
    sp3 = sdf.find(',only')
    sp3d = sdf.find('!')
    sp3a = sdf.find(')-')
    sp3b = sdf.find(')')
    sp3c = sdf.find('))-')
    if sp3 > -1 and sp3a > -1:
        sp2a = sdf[:sp3]
        sp2b = sdf[sp3a:]
        sdf = sp2a + sp2b
    elif sp3d > -1 and sp3a > -1:
        if sp3d < sp3a:
            sdf1 = sdf[:sp3d]
            sdf2 = sdf[sp3b:]
            sdf = sdf1 + sdf2
    elif sp3b > -1 and sp3a == -1 and sp3 == -1:
        sdfa = sdf.replace(')', ')-xxx')
        sdf = sdfa
    elif sp3c > -1:
        sdf = sdf.replace('))-', ')-')
    sdf2 = sdf.replace(')-', '-')
    sdf3 = sdf2.replace('(', '-')
    sdf4 = sdf3.split('-')
#
    if len(sdf4) == 4:
        sdfn = sdf4[3]
        sdfp3 = sdf4[1]
        sdfp2 = sdf4[2]
        sdfp = sdf4[0]
        return(sdfp, sdfp2, sdfn, sdfp3)
    elif len(sdf4) == 3:
        sdfn = sdf4[2]                   # line number
        sdfp = sdf4[0]                   # calling func / sub etc
        sdfp2 = sdf4[1]                  # internal function / subroutine
        return(sdfp, sdfp2, sdfn)
#
##############################################################################
#
#
#   Purpose  :
#   Input    :
#              dzi     => switch indicating dictionary
#              dz1     => dictionary *.f90 data
#              dz2     => dictionary *.f90 interface data
#              dz3     => dictionary *.f90 module data
#              dz4     => dictionary *.c data
#              dz5     => dictionary *.f90 function data
#              dzcf    => dictionary * .called by data
#              tcb     => http link passed from shell
#              tcb2    => directory  passed from shell
#              dpl     => List of links to dependency diagrams
#
#   Output   : html code cover page
#
##############################################################################


def recon3(dzi, dz1, dz2, dz3, dz4, dz5, dzcf, tcb, tcb2, dpl):
    if dzi == 1:
        dzn = dz1
        dzp = readd(dzn, dz1, dz2, dz3, dz4, dz5, dzcf, tcb, tcb2, dpl)
    elif dzi == 3:
        dzn = dz3
        dzp = readd(dzn, dz1, dz2, dz3, dz4, dz5, dzcf, tcb, tcb2, dpl)
    elif dzi == 4:
        dzn = dz4
        dzp = readd(dzn, dz1, dz2, dz3, dz4, dz5, dzcf, tcb, tcb2, dpl)
    elif dzi == 5:
        dzn = dz5
        dzp = readd(dzn, dz1, dz2, dz3, dz4, dz5, dzcf, tcb, tcb2, dpl)
    return()
#
#
##############################################################################
#
#
#               Each i/p dictionary will be of the form :
#
#               odx[key][0] = ['full code name']
#               odx[key][1] = ['code listing']
#               odx[key][2] = ['list of Mod interface files']
#               odx[key][3] = ['list of Internal Subroutines']
#               odx[key][4] = ['List of Internal Functions']
#               odx[key][5] = ['list of calls to external Subroutines' ]
#               odx[key][6] = ['Module - Subroutines calls ']
#               odx[key][7] = ['Module - Function calls']
#               odx[key][8] = ['*.c subroutine / function calls ']
#               odx[key][9] = ['*.F90 function calls ']
#               odx[key][10] = ['Called by ']
#
#
#
##############################################################################


def readd(dxz, dx1, dx2, dx3, dx4, dx5, dxcb, rlnk, rlnk2, dplf):
    ndx = {}
    for kx in list(dxz.keys()):
        mlist = []
        dxf2 = []
        dxf3 = []
        dxf4 = []
        dxf5 = []
        dxf6 = []
        dxf7 = []
        dxf8 = []
        dxf9 = []
        dxf10 = []
        mdx1 = []
        blist = []
        dpx = ''
        dpx = dxz[kx][0][0]
        sn = 4
        mnlk = clinkn(dxz, kx, dpx, rlnk, sn)
        mdx1.append(kx)
        mdx1.append(mnlk)
        mlist.append(mdx1)
#
        nlz = 2
        ltin = dxz[kx][2]
        cdict = dx2
        ln2 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln2)
#
        nlz = 3
        ltin = dxz[kx][3]
        cdict = dxz
        ln3 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln3)
#
        nlz = 4
        ltin = dxz[kx][4]
        cdict = dxz
        ln4 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln4)
#
        nlz = 5
        ltin = dxz[kx][5]
        cdict = dx1
        ln5 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln5)
#
        nlz = 6
        ltin = dxz[kx][6]
        cdict = dx3
        ln6 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln6)
#
        nlz = 7
        ltin = dxz[kx][7]
        cdict = dx3
        ln7 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln7)
#
        nlz = 8
        ltin = dxz[kx][8]
        cdict = dx4
        ln8 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln8)
#
        nlz = 9
        ltin = dxz[kx][9]
        cdict = dx5
        ln9 = start_def(ltin, cdict, nlz, rlnk)
        mlist.append(ln9)
#
        if kx in list(dxcb.keys()):
            mlist.append(dxcb[kx])
        if len(mlist) < 10:
            blist = []
            mlist.append(blist)
#
        dpfg = rlnk + kx + '_depend.html'
        dpf = kx + '_depend'
        lnlist = []
        dq1 = ''
        sfz = ''
        sz = 99
        dlnk = dxz[kx][0][0]
        if dpfg in dplf:
            sdl = '<a href=\"' + dpfg + '\">' + dlnk + '</a>'
            lnlist.append(sdl)
        mlist.append(lnlist)
        ndx[kx] = mlist
        drig2 = collections.OrderedDict(sorted(ndx.items()))
        drz1 = rlnk2 + kx + "_Cover.html"
        topz1 = 'file_cover.html'
        cref1 = 'MetDB Code Register'
        tq1 = env.get_template(topz1)
        outputx1 = tq1.render(clist=mlist)
        with open(drz1, 'w') as fm1:
            fm1.write(outputx1)
        fm1.close()
    return()
#
##############################################################################
#
#   dcl  => input list
#   dcp2 => comparison dictionary
#   nref => element no
#   rnk  => directory path to add to link
#
#
##############################################################################


def start_def(dcl, dcp2, nref, rnk):
    #
    nlist = []
    for xz in range(0, len(dcl)):
        if nref >= 2 and nref <= 5 or nref == 9:
            (vp1, vp2, vp3) = spstring(dcl[xz])
            vp4 = ''
        else:
            (vp1, vp2, vp3, vp4) = spstring(dcl[xz])
        swt = 4
    #
        if nref == 2 or (nref >= 5 and nref <= 8):
            if nref == 5 or nref == 2:
                vplnk = vp2
            else:
                vplnk = vp4
            dpn = dcl[xz].split('-')
            dpname = dpn[0]
            tlnk = clinkn(dcp2, vplnk, dpname, rnk, swt)
        else:
            vplnk = vp1
            dpn = dcl[xz].split('-')
            dpname = dpn[0]
            tlnk = clinkn(dcp2, vplnk, dpname, rnk, swt)
            tlnk2 = tlnk.strip()
            if len(tlnk2) == 0:
                vplnk = vp2
                tlnk = clinkn(dcp2, vplnk, dpname, rnk, swt)
    #
        if tlnk not in nlist:
            nlist.append(tlnk)
    return(nlist)
#
#
##############################################################################
#
#  def     : detail_dep
#
#  purpose : Expand the list of dependencies for each code listing
#            supplied in the input dictionary to include sub-dependencies.
#            The nesting level of each dependencie in relation to the
#            calling function / subroutine will be indicated by 'code_name-n'
#            where '-n' is an integer.
#
#  input   : agh => indict: Dict of nested lists to which a seperate
#            list of code dependencies have been added
#            code  :{ [['code.f90'], ['listing of the subroutine '],
#            ['findpath', 'readcf', 'value', 'mdb_abort']]}
#
#            dlp => Entry to be expanded of the form
#            code  :{ [['code.f90'], ['listing of the subroutine '],
#            ['findpath', 'readcf', 'value', 'mdb_abort']]}
#
#  output  : result2 => indict: Dict of nested lists to which a seperate
#            list of code dependencies have been added
#            code  :{ [['code.f90'], ['listing of the subroutine '],
#            ['airloc-1', 'airgrp-1', 'becpos-1', 'becret-2', 'dsinfo-3',
#            'dsopen-4', 'satype-4', 'findpath-4', 'satype-2']}
#
#
##############################################################################


def detail_dep(agh, dlp, dlm):
    #
    xpout = []
    bls = dlp[2]
    sd = dlp[0][0].find('.')
    sdf = dlp[0][0][:sd].lower()
    if len(bls) == 0:
        results2 = []
        return(results2)
    else:
        lv = 0
        xpout = []
    #   iterate through the length of the bls list
    #   check for the lack of  a '-' in the entry
    #   if not found append a '-n' to the entry
    #   append the modified entry to the output list
    #   check the new entry is not a '_mod' file.
    #   '_mod' files will not have dependencies
    #   if dependencies found append to output list
    #
        while True:
            lv = lv + 1
            for xq in range(0, len(bls)):
                if bls[xq].find('-') == -1:
                    sdf = bls[xq]+'-'+str(lv)
                    sdfr = bls[xq]
                    xpout.append(sdf)
                    if sdfr.find('_mod') == -1:
                        if sdfr in list(agh.keys()):
                            dltr = agh[sdfr][2]
                            if len(dltr) > 0:
                                xpout.append(dltr)
                else:
                    xpout.append(bls[xq])
    #
    #   the following will:
    #   1. convert the nested string to a string
    #   2. remove internal parenthesis '[' ']'
    #   3. replace multiple whitespace with single
    #   4  remove ' ' '
    #   5 split string to list on ','
    #
            xpouts = str(xpout)
            xpout1 = xpouts.replace('[', '')
            xpout2 = xpout1.replace(']', '')
            xpout3 = xpout2.replace(' ', '')
            xpoutsf = xpout3.replace('\'', '')
            xpout4 = xpoutsf.split(',')
            t = 1
            for xw in range(0, (len(xpout4))):
                if xpout4[xw].find("-") == -1:
                    t = 0
                    break
            bls = xpout4
            xpout = []
            if t == 1:
                break
        result2 = bls
        xpout = []
    return(result2)
#
##############################################################################
#
#  def     : config_dep
#
#  Purpose : construct dependency dictionary and output to diagram
#
#  Input:    drx  => Input dictionary (currently limited to *.f90 files)
#            dlx  => Output link
#                    http://www-mdb-apps-test/code_dep/output/
#
#            dlx2 => Template reference dir
#                    /var/moods/code_register/template_ref/
#
#            dlx3 => Output directory
#                    /var/www/html/code_dep/output/
#
#  Output:   dpdg => List containing links to completed dependency diagrams
#
##############################################################################


def config_dep(drx, dlx, dlx2, dlx3):
    #
    # reconfigure the input dictionary for submission to detail_dep
    #
    dpdg = []
    drxot = {}
    for cd in list(drx.keys()):
        oplist = []
        foplist = []
        nlist0 = drx[cd][0]
        nlist1 = drx[cd][1]
        nlist5 = drx[cd][5]
        if len(nlist5) > 0:
            foplist.append(nlist0)
            foplist.append(nlist1)
            for xz in range(0, len(nlist5)):
                (xv1, xv2, xv3) = spstring(nlist5[xz])
                if xv2 in list(drx.keys()) and xv2 not in oplist:
                    if xv2 != cd:
                        oplist.append(xv2)
            foplist.append(oplist)
            drxot[cd] = foplist
    #
    lnd = {}
    dlp = ''
    for kf1 in list(drxot.keys()):
        xpn = drxot[kf1]
        xpf = drxot[kf1][0]
        expent = detail_dep(drxot, xpn, dlp)
        # configure dimensions of the output array
        nrows = len(expent)
        nvf = 0
        nvq = []
        expent2 = []
        for nv in range(0, len(expent)):
            nvs = expent[nv].split('-')
            if nvs[0] in list(drx.keys()):
                xval = drx[nvs[0]][0][0]
                sx1 = '-'
                sx3 = '-'
                sx4 = nvs[0]
                sx2 = xval
                sw = 4
                xfline2 = clinkn(drx, sx4, sx2, dlx, sw)
                xvalf = xfline2 + '!' + nvs[1]
                expent2.append(xvalf)
            nvn = int(nvs[1])
            if nvn > nvf:
                nvf = nvn
        nvq.append(nvf)
        nvq.append(nrows)
        topd1 = 'depend-1.html'
        topz1 = env.get_template(topd1)
        outputx1 = topz1.render(keyz=kf1, keyf=xpf,
                                clist=expent2, dims=nvq)
        drx1 = dlx3 + kf1 + "_depend.html"
        drx2 = dlx + kf1 + "_depend.html"
        if drx2 not in dpdg:
            dpdg.append(drx2)
        with open(drx1, 'w') as fmx:
            fmx.write(outputx1)
        fmx.close()
    return(dpdg)
    #
##############################################################################
#
#  def     : cover_sum
#
#               Directory of lists for the *.f90 code
#               Directory of lists for the  module  code
#               Directory of lists for the *_mod.f90 interface code
#               Directory of lists for the *.c code
#               Directory of lists for the *_mod.f90 functions
#
#
##############################################################################


def cover_sum(xd1, xd2, xd3, xd4, xd5, dln, dln2, tsmp):
    #
    # re - define the input dictionaries for output
    #
    sref = set(string.ascii_lowercase)
    srefl = list(sref)
    srefu = sorted(srefl)
    srefu2 = [x.upper() for x in srefu]
    srefu = srefu2
    nrefu1 = []
    nrefu2 = []
    for xz in range(0, len(srefu)):
        ipn = srefu[xz]
        ipf1 = '<a href=\"' + dln + 'master_list.html#' +\
               ipn + '\">' + ipn + '</a>'
        nrefu1.append(ipf1)
        alnk = '<a name=\"' + ipn + '\"></a>' + ipn
        nrefu2.append(alnk)
        #
    tlist = ['*.F90 code ', '*_mod.f90 interface ',
             '*.f90 module', '*.c ', '*_mod.f90 functions']
    oxfa = {}
    print "range srefu equals ",len(srefu),"\n"  
    print "srefu -2 ",srefu,"\n"
    for xf in range(0, len(srefu)):
        print "srefu[xf] ",srefu[xf],"\n" 
        ox1 = []
        ox1.append(tlist[0])
        swt = 5
        oxz1 = cover_comp(xd1, srefu[xf], ox1, dln, swt)
        ox2 = []
        swt = 1
        ox2.append(tlist[1])
        oxz2 = cover_comp(xd2, srefu[xf], ox2, dln, swt)
        ox3 = []
        swt = 5
        ox3.append(tlist[2])
        oxz3 = cover_comp(xd3, srefu[xf], ox3, dln, swt)
        ox4 = []
        swt = 5
        ox4.append(tlist[3])
        oxz4 = cover_comp(xd4, srefu[xf], ox4, dln, swt)
        ox5 = []
        swt = 5
        ox5.append(tlist[4])
        oxz5 = cover_comp(xd5, srefu[xf], ox5, dln, swt)
        nlnk = []
        nlnk.append(nrefu2[xf])
        oxf = []
    #
        oxf.append(oxz1)
        oxf.append(oxz2)
        oxf.append(oxz3)
        oxf.append(oxz4)
        oxf.append(oxz5)
        oxf.append(nlnk)
    #
        keyf = srefu[xf].upper()
        oxfa[keyf] = oxf
    #
        tmpt = 'Output Created - ' + tsmp
        dfx = sorted(oxfa.keys())
        ctemp = 'Main_cover.html'
        topz1 = env.get_template(ctemp)
        outputx1 = topz1.render(rflist=nrefu1, dref=oxfa,
                                dlist=dfx, dlist2=nrefu1,
                                dlist3=nrefu2, tsd=tmpt)
        drx1 = dln2 + "master_list.html"
        with open(drx1, 'w') as fmx:
            fmx.write(outputx1)
    return()
    #
##############################################################################
#
#     def:    cover_comp
#
#             xdx    =>   reference dictionary
#             srefx  =>   Alphabetical list
#             oxz    =>   List containg title as first element
#             dnx    =>   link, passed to clinkn
#             swp    =>   switch 1 - 5
#
#             called by   Def: cover_sum
#
#             calls:      clinkn
#
##############################################################################


def cover_comp(xdx, srefx, oxz, dnx, swp):
    #
    for keyx in list(xdx.keys()):
        trefx = keyx[0]
        if srefx.lower() == trefx.lower():
            tvalx = xdx[keyx][0][0]
            sfg = ''
            tvalf = clinkn(xdx, keyx, tvalx, dnx, swp)
            oxz.append(tvalf)
    oxzf = sorted(oxz)
    if len(oxzf) < 6 and len(oxzf) > 0:
        ade = 6 - (len(oxzf))
        for xy in range(0, ade):
            oxzf.append(' ')
    flenz = len(oxzf)-1
    oxzf.append(flenz)
    #
    return(oxzf)
    #
##############################################################################
#
# main (calling function)
#
##############################################################################


now = datetime.datetime.now()
y1 = now.year
mn1 = now.month
d1 = now.day
h1 = now.hour
m1 = now.minute
s1 = now.second
mn1 = str(mn1).zfill(2)
d1 = str(d1).zfill(2)
h1 = str(h1).zfill(2)
s1 = str(s1).zfill(2)
y1s = str(y1)
fstx = d1 + '-' + mn1 + '-' + y1s
#
tstamp = "Last updated - " + str(y1) + "-" + str(mn1) + "-" +  \
          str(d1) + " " + " " + str(h1) + ":" + str(m1) + ":" + str(s1)
#
mdfp = sys.argv[1:]
sdata = mdfp[0]           # metdb-repo input directory
dlink = mdfp[1]           # http://www-mdb-apps-test/code_dep/output/
tref = mdfp[2]            # /var/moods/code_register/template_ref/
dlink2 = mdfp[3]          # /var/www/html/code_dep/output/
#
(ld1, ld2, ld3, d1o, d2o) = set_d(sdata)
#
switch = 1   # *.f90 & *.f90 functions
(m901, m905) = list_files(ld1, switch)
#
switch = 2   # *_mod interface & *_mod modules
(m902, m903) = list_files(ld3, switch)
#
switch = 3   # *.c
(m904) = list_files(ld2, switch)
#
(dr1, dr2, dr3, dr4, dr5) = tree_dr(m901, m902, m903, m904, m905)
#
drpx = config_dep(dr1, dlink, tref, dlink2)  # output dependency diagram
#
(cb1, cb3, cb4, cb5) = callby(dr1, dr3, dr4, dr5, dlink)
#
sx = 1
(oz1) = recon(dlink, dr1, dr2, dr3, dr4, dr5, sx)
ftl_out(oz1, dlink, dlink2)
#
sx = 2
(oz2) = recon(dlink, dr1, dr2, dr3, dr4, dr5, sx)
ftl_out(oz2, dlink, dlink2)
#
sx = 3
(oz3) = recon(dlink, dr1, dr2, dr3, dr4, dr5, sx)
ftl_out(oz3, dlink, dlink2)
#
ftl_out(dr4, dlink, dlink2)
#
sx = 5
(oz5) = recon(dlink, dr1, dr2, dr3, dr4, dr5, sx)
ftl_out(oz5, dlink, dlink2)
#
dxp = 1   # *.f90
recon3(dxp, dr1, dr2, dr3, dr4, dr5, cb1, dlink, dlink2, drpx)
#
dxp = 3   # *.f90 modules
recon3(dxp, dr1, dr2, dr3, dr4, dr5, cb3, dlink, dlink2, drpx)
#
dxp = 4   # *.c
recon3(dxp, dr1, dr2, dr3, dr4, dr5, cb4, dlink, dlink2, drpx)
#
dxp = 5   # *.f90 functions
recon3(dxp, dr1, dr2, dr3, dr4, dr5, cb5, dlink, dlink2, drpx)
#
ofd = cover_sum(dr1, dr2, dr3, dr4, dr5, dlink, dlink2, tstamp)
