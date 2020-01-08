#!/usr/bin/python
# -*- coding: iso-8859-1 -*-
import subprocess
from datetime import datetime
import time
import sys
import string
import collections
import operator
from operator import itemgetter
from subprocess import Popen
import os
import os.path
import re
#
##############################################################################
#       Name -      BUFR_Tables.py
#
#       Author -    Richard Weedon
#
#       Date -      11th April 2016..
#
#       Description.
#    1. Add new Version of Global Table B (requires txt version of Table B,
#       extracted from the WMO zip folder)
#    2. Add new Version of Global Table D (requires txt version of Table D,
#       extracted from the WMO zip folder)
#    3. New Table B Local Entry (will be added to current operational table)
#    4. New Table D Local Entry (will be added to current operational table)
#    5. Compare new and old Table B revisions (requires txt version of
#       Table D, extracted from the WMO zip folder)
#    6. Compare new and old Table D Revisions (requires txt version of
#       Table D, extracted from the WMO zip folder)
#    7. Add new version of BUFR Codefigs.
#    8. Expand WMO D Sequence to constituent B Descriptors.
#    9. Expand WMO D Sequence and produce outline for a MetDB element Index.
#   10. Exit / Quit
#
#   Note:
#   Options 1 - 4 & 6 - 9 will use wget to extract the latest versions of
#   the BUFR and / or
#   Codefig tables from the WMO Code Tables Web Site. In addition the current
#   operational versions
#   of the BUFR and Codefig tables will be copied (scp) from MDBDB-PREPROD.
#   To complete this the user must have access to the moodsf account on the
#   MetDB.
#
#   Option 10 will run a full decode of all files (MHSR) found in the
#   depository directory (~/decode_in/)
#
#
#
#       Subroutine description  .
#
#         Name                                            Purpose
#
#   -------------------------------------------------------------------------
#
#     table_add(Tno)         Called by Menu(), this will initiate a general
#                            prompt
#
#                            for the name of the input files etc.
#
#                            input   - (Tno) option identifier (1 - 10)
#
#                            output  - Varies according to the option called.
#
#                            tablex  - name and location of BUFR XML file
#                                     (Either B or D)
#                            tableo  - name and location of BUFR txt file
#                                     (Either B or D)
#                            tableq  - name and location of BUFR Table C
#
#                            seq_an  - Table B or D sequence identifier
#
#   --------------------------------------------------------------------------
#     add_entry(w,v)         The manual entry of either Table B Descriptors
#                            and  / or Table D Sequences.
#
#                            add_entry(w,rev)
#
#                            w    -   switch to define the table updated
#                                     (either  B or D)
#
#                           rev   -   revision no of current BUFR table
#
#
#   --------------------------------------------------------------------------
#
#     finaldictb(masterl,masterg)      Will merge master dicts output by
#                                      GetDatb and read_in_tableb into a
#                                      single final dict. Where entrys from
#                                      the new (WMO) txt tables are found in
#                                      the existing tables a new revision will
#                                      be added. Where new entries are found
#                                      without a corresponding entry in the
#                                      existing table a new singlar entry
#                                      will be made. Each entry in the
#                                      merged dict is then sorted by the
#                                      revision no in ascending order
#                                      The presence of previous versions
#                                      is indicated by placing a plus
#                                      againt the latest version no of
#                                      any entry
#
#                                      input   - masterl  Dictionary
#                                                containing nested lists
#                                                of the existing operational
#                                                entries
#                                      input   - masterg  Dictionary new
#                                                entries parsed from the
#                                                WMO xml tables
#                                      output  - fdict  Merged dict
#                                                derived from the local
#                                                and global dicts
#
#   --------------------------------------------------------------------------
#
#     finaldictd(masterl,masterg)      Will merge master dicts output by
#                                      GetDatd and read_in_tabled into a
#                                      single final dict.
#                                      Where entrys from the new (WMO) txt
#                                      tables are found in the existing table
#                                      the new revision will replace the old
#                                      Where new entries are found without
#                                      a corresponding entryb in the existing
#                                      table
#                                      a new (singlar) entry will be made.Each
#                                      entry in the merged dict is then sorted
#                                      revision no (in ascending order).
#
#                                      input  - masterl - Dictionary containing
#                                                         nested lists
#                                                         of the existing
#                                                         operational systems.
#                                      input  - masterg - Dict new entries
#                                                         parsed from the (WMO)
#                                                         txt tables.
#                                      output - fdict   - Merged dict derived
#                                                         from the local and
#                                                         global dicts.
#
#   ---------------------------------------------------------------------------
#
#     getDatB(tablenb)                 Will parse and read into a dictionary
#                                      the contents of the new (WMO) txt
#                                      version.
#                                      B. Each descriptor key will
#                                      reference a series of lists as outlined
#                                      below -
#
#                                      dict[key]=[Descriptor Name,unit,scale,
#                                              ref,width,cunit,cscale,cwidth]
#
#                                      input -   tablenb   - Name of the new
#                                                            BUFR table B
#                                                            ((WMO) txt).
#                                      output -  tableout  - Master dict for
#                                                            new BUFR TableB
#  (excluding local descriptors)
#                                                version   - Revision No of
#                                                            the new BUFR table
#
#   ---------------------------------------------------------------------------
#
#     getDatD(tablend)                 Will parse and read into a dictionary
#                                      the contents of the new (WMO) version
#
#                                      reference a series of lists as
#                                      outlined below -
#                                      dict[key]=[Descriptor1,Descriptor2,
#                                      dict = Descriptor3,Descriptor4,]
#
#
#                                      input -   tablend   - Name of the
#                                                new BUFR table D ((WMO) txt).
#                                      output -  tableout  - Master dict for
#                                                the new tabled BUFR
#                                                Table D excluding
#                                                local sequences
#                                                version - Revision No of the
#                                                new BUFR table
#
#   ---------------------------------------------------------------------------
#
#     read_in_tableb(tablename)        This subroutine will read into memory
#                                      the contents of the operational version
#                                      of BUFR tableB. The file will contain
#                                      multiple versions of the same for a
#                                      specific time.the entry must be
#                                      descriptors. The data will be read into
#                                      a dict of lists.
#                                      An entry for any descriptor will be of
#                                      the form -
#
#                                      dict[010114][[rev(1),Descr,unit,scale,
#                                      ref val, data width],[rev(2),Descr,unit
#                                      scale,ref val,data width]]
#
#                                      The nested list structure is sorted in
#                                      ascending order of the revision no
#                                      Before being assigned to the dict.
#
#                                      input -   Tablename - Name of the
#                                                operation BUFR table B (txt
#                                                format).
#                                      output -  Master    - Master dict
#                                                for current (operational) BUFR
#                                                rev_line  - Revision No of the
#                                                 current operational table
#
#   ---------------------------------------------------------------------------
#     read_in_tabled(tablename)        This subroutine will read into memory
#                                      the contents of the operational version
#                                      of BUFR tableD. Unlike TableB
#                                      the file will not contain multiple
#                                      versions of the same . Each entry will
#                                       comprise a single list of the form -
#                                      dict[307079]["Title","count",
#                                      # Descriptor1,Descriptor2,.....]
#
#                                      input -   Tablename - Name of the
#                                                operation BUFR table B
#                                               (txt format).
#                                      output -  Master    - Master dict for
#                                                current (operational) BUFR
#                                                rev_line  - Revision No of
#                                                The current operational table
#
#   --------------------------------------------------------------------------
#
#     output_b(f_dict,N_ver,O_ver)     Will output the final (merged) dict
#                                      in the current operational format.
#
#                                      input -
#                                      f_dict  - final merged containing
#                                                dictionary nested lists.
#
#                                      N_ver   - Local Version of the
#                                                current operational tables.
#                                      N_ver   - WMO Version of
#                                                the current operational
#                                      output -  The filename of the final
#                                                output will be of the form
#
#                                       BUFR_TableB_YYYY_MM_DD_HH:MM:SS.txt.
#
#   ---------------------------------------------------------------------------
#
#     output_d(f_dict,N_ver,O_ver)     Will output the final (merged) dict
#                                      in the current operational format.
#
#                                      input  -  f_dict  - final merged
#                                                dictionary (containing
#                                                nested lists).
#                                      N_ver  -  Local Version of the
#                                                current operational tables.
#                                      N_ver  -  WMO Version of the current
#                                                operational tables .
#                                      output -  The filename of the final
#                                                output will be of the form
#
#                                      BUFR_TableD_YYYY_MM_DD_HH:MM:SS.txt.
#
#   ---------------------------------------------------------------------------
#
#     comp_versions((nvers,overs,table,wmo_rev,loc_rev,hdiri)
#
#                             Run comparison of two dicts constructed
#                             from the new and old versions of BUFR
#                             Table B or D.
#
#                             input - nvers - new version of the
#                                             BUFR Table
#                             overs - old version of the
#                                             BUFR table
#                                              table - Switch to signify the
#                                                      table type (B or D)
#
#                             output -TableD_Comparison_YY-MM-DD_HH:MM:SS.txt
#                                     TableB_Comparison_YY-MM-DD_HH:MM:SS.txt
#
#
#   ---------------------------------------------------------------------------
#
#     read_oldfig(tablename)           Read in the current (operational) BUFR
#                                      Codefigs to a dict.
#
#                                      input  tablename - Current (operational)
#                                                         version of codefigs.
#
#                                      output fin_dict  - Dict of lists
#                                                         containing current
#                                                         (MetDB) CodeFig
#                                                         The dict will be of
#                                                          the structure -
#
#     Dict[Copde Table No] =  [[Title][Entry No, Entry],[Entry No, Entry]...]
#
#     rev_no        - Revision No of the current version of codefigs
#
#   ---------------------------------------------------------------------------
#
#     read_newfig(tablename)
#
#          Read in the WMO version of BUFR Codefigs
#          (.txt) to a dict.
#
#          input:   Tablename - WMO Codefigs (.txt)
#
#          output:  fin_dict  - Dict of lists containing current (MetDB)
#                                CodeFig tables.
#
#          rev_no             - Revision No of the current version
#                                of codefigs
#
#          The output dict structured as follows -
#
#          Dict[Copde Table No]=[[Title][Entry No, Entry],[Entry No, Entry]...]
#
#
#   --------------------------------------------------------------------------
#
#     mdict(masterld,mastergd)
#
#           merge WMO and MetDB versions of the BUFR Codefig tables
#
#           input:      Dict  masterld     -  local version of the  codefig
#                                             tables
#
#                       Dict  mastergd     -  WMO Version (New) version of
#                                             the code tables
#
#           output:  -  Merged data in a dictionary
#
#   ---------------------------------------------------------------------------
#
#     out_codefig(mfdict)
#
#           output BUFR Codefig files
#
#           input:     mfdict (dict output by mdict)
#                      codefig tables
#
#           output:    CodeFig_YY-MM-DD_HH:MM:SS.txt
#
#   ---------------------------------------------------------------------------
#
#     link_common_tables(mfdict)
#
#           Cross refence the entries in the merged data dictionary for
#           codefigs with the Common Code Tables
#
#           input:   mfdict     -  Merged codefig tables
#
#           output:  mdict_out  -  Merged codefig tables with entries
#                                  from the common code tables.
#
#   ---------------------------------------------------------------------------
#
#     out_codefig(mfdict)
#
#           output BUFR Codefig tables to file
#
#           input:  mfdict  -  Merged codefig tables
#
#           output: BUFR CodeFig Table - CodeFig_YY-MM-DD_HH:MM:SS.txt
#
#   ---------------------------------------------------------------------------
#
#     comp_code_dicts
#
#           comparison of the new and old codefig files
#
#           input:  dict1     -  Current MetDB codefig tables
#                                (dictionary of lists)
#
#                   dict2     -  New Code Fig tables downloaded from the
#                                WMO Web Pages
#
#           output: Nil
#
#   ---------------------------------------------------------------------------
#
#     rep_d
#
#           Expand all D sequences, replacing nested D sequences with full
#           listings of B Descriptors
#
#           input:  odictn  -  Dict of D Sequences.
#
#           output: odictf  -   Dictionary containing expanded D sequence.
#
#   ---------------------------------------------------------------------------
#
#     print_list
#
#           Subroutine called by "straight" to identify and replace nested
#           lists. The instance method "is instance" will return TRUE if the
#           object matches the type specified, in this case "List".
#           Therefore a nested list such as -
#
#           ["23","24","25",["26","27","28"],"29","30"]
#           will be transformed to
#           ["23","24","25","26","27","28","29","30"]
#
#           input:    the_list -  list which may (or may not)
#                      contain nested lists
#
#           Output:  dn          -  output lists
#
#   --------------------------------------------------------------------------
#
#     straight(idict)
#
#           Will search for nested lists (within the main list) and merge
#           the nested data at the right point. The end result will be
#           one continous list. Note this subroutine includes a call
#           to the subroutine print_list.
#
#           input:   idict -  expanded D sequence returned by rep_d.
#
#           returns:  tempd - dictionary of lists corresponding to an
#                             expanded Table D Sequence cross referenced
#                             against Tables B & C.
#
#   --------------------------------------------------------------------------
#
#     read_tablec(cin)
#
#           read in the contents of BUFR Table C to a dictionary of lists.
#           The dictionary will be of the format -
#
#           tabc{key :[ 'Descriptor' ,'Details',' ',' ']}
#
#           input:     cin  -  text version of Table C
#
#           returns:   tabc -  dictionary of lists corresponding to Table C
#
#   --------------------------------------------------------------------------
#
#     cross_ref(bdictin,cdictin,ddictin)
#
#           cross_reference an expanded D sequence against BUFR Tables B & C
#
#           input:  bdictin    -  dictionary of lists containing Table B
#                   ddictin    -  dictionary of lists containing Table D
#                   cdictin    -  dictionary of lists containing Table C
#
#           returns: tempd     -  dictionary of lists corresponding to an
#                                 expanded Table D Sequence cross
#                                 referenced against Tables B & C.
#
#   --------------------------------------------------------------------------
#
#     out_seq(fdict,SeqID)
#
#           Output Expanded D Sequence
#
#           Input: fdict  -   Expanded D Sequence , cross referenced
#                             against Table C. Note this is output
#                             from cross_ref.
#
#           seqID         -   D Sequence which has been expanded.
#
#           Output:       -   D_Sequence_Sequence_ID_output_YYDDMM_HHMM.txt
#
#   ---------------------------------------------------------------------------
#
#     index_cr
#           Output skeleton of the elements index for a specific D sequence
#
#           input:  fdict   -  Expanded D Sequence , cross referenced against
#                              Table C. Note this is output from cross_ref
#
#                   seqID   -  D Sequence which has been expanded.
#
#                   output:    Element_Index_SeqID_output_YYMMDD_HHMM.txt
#
##############################################################################
# subroutine name: table_check
#
# purpose:         check for the presence of the tables. Failure to locate
#                  the required table(s) will result in the program exit.
#
# input:
#                  opt       -   option passed from calling BASH shell.
#                  hdir      -   root of the bufr directorys
#                  tablexf   -   table1
#                  tableof   -   table2
#                  tableqf   -   table3
#
# output:          Nil (will exit if tables not found)
#
#
##############################################################################


def table_check(opt, hdir, tablexf, tableof, tableqf):
    if opt == "1" or opt == "3" or opt == "6":
        if not os.path.isfile(tablexf):
            print("BUFR_Tables: Global BUFR TableB not found at =>  ", tablexf)
            exit()
        if not os.path.isfile(tableof):
            print("Local BUFR TableB not found at =>   ", tableof)
            exit()
    elif opt == "2" or opt == "4" or opt == "7":
        spathx = hdir+"/BUFR_d/"+tablexf
        spatho = hdir+"/op/"+tableof
        if not os.path.isfile(tablexf):
            print("BUFR_Tables: Global BUFR TableD not found at =>  ", spathx)
            exit()
        if not os.path.isfile(tableof):
            print("Local BUFR TableD not found at =>   ", spatho)
            exit()
    elif opt == "5":
        if not os.path.isfile(tablexf):
            print("BUFR_Tables: Global Common Code Table not found at =>  ", \
                tablexf)
            exit()
        if not os.path.isfile(tableof):
            print("Local Common Code Table not found at =>   ", tableof)
            exit()
    elif opt == "8" or opt == "9":
        if not os.path.isfile(tablexf):
            print("BUFR_Tables: Global BUFR TableD not found at  =>  ", tablexf)
            exit()
        if not os.path.isfile(tableof):
            print("BUFR_Tables: Global BUFR TableB not found at =>   ", tableof)
            exit()
        if not os.path.isfile(tableqf):
            print("BUFR_Tables: Global BUFR TableC not found at =>   ", tableqf)
            exit()
    return()
#
###############################################################################
# subroutine name: add_entry
#
# purpose:         Manually add new entrys to either BUFR Tables B or D
#
# input:           w            -   switch to define the table updated
#                                   (either B or D)
#                  rev          -   revision no of current BUFR table
#
# output:          final        -   New Descriptor(s) / sequence(s)
#                                   as a BUFR DICT
#
###############################################################################


def add_entry(w, rev, master_l):
    if w == "B":
        y = input("No of Table B Descriptors to be created? :  ")
        y2 = int(y)
        for x in range(0, y2):
            row = []
            row2 = []
            xn = x+1
            xns = str(xn)
            desn = "FXY of Descriptor "+xns+" : "
            while True:
                fxy = input(desn)
                questionfxy = fxy+" - entered for descriptor identifier\n"
                print(questionfxy)
                q2p = input("is this correct?: (Y to proceed , \
                                N to re-enter the Identifier or Q to Quit) ")
                q2p = q2p.upper()
                if q2p == "Y" or q2p == "Q":
                    if q2p == "Y":
                        break
                    if q2p == "Q":
                        exit()
            descf = "Description of "+fxy+" (60 Chars or less):  "
            while True:
                desc = input(descf)
                questiondesc = " "+desc+" - entered for description of " \
                               + fxy+"\n"
                print(questiondesc)
                q3p = input("is this correct?: (Y to proceed , \
                                 N to re-enter the Description or Q to Quit) ")
                q3p = q3p.upper()
                if q3p == "Y" or q3p == "Q":
                    if q3p == "Y":
                        break
                    if q3p == "Q":
                        exit()
            print("++++++++++++\n")
            unitf = "Unit of " + fxy + " : "
            while True:
                unit = input(unitf)
                questionunit = " " + unit + " entered as \
                                the unit of " + fxy + "\n"
                print(questionunit)
                q4p = input("is this correct?: (Y to proceed , \
                                N to re-enter the Unit or Q to Quit) ")
                q4p = q4p.upper()
                if q4p == "Y" or q4p == "Q":
                    if q4p == "Y":
                        break
                if q4p == "Q":
                    exit()
            print("++++++++++++\n")
            scalef = "Scale of " + fxy + " : "
            while True:
                scale = input(scalef)
                questionscale = " " + scale + " entered as the \
                                Scale of " + fxy + "\n"
                print(questionscale)
                q5p = input("is this correct?: (Y to proceed , \
                                N to re-enter the Scale \
                                value or Q to Quit) ")
                q5p = q5p.upper()
                if q5p == "Y" or q5p == "Q":
                    if q5p == "Y":
                        break
                    if q5p == "Q":
                        exit()
            print("++++++++++++\n")
            reff = "Reference Value of "+fxy+" : "
            while True:
                refer = input(reff)
                questionrefer = " " + refer + " entered as the \
                                 Reference value for " + fxy + "\n"
                print(questionrefer)
                q6p = input("is this correct?: (Y to proceed \
                                  ,N to re-enter the \
                                  Reference value or Q to Quit) ")
                q6p = q6p.upper()
                if q6p == "Y" or q5p == "Q":
                    if q6p == "Y":
                        break
                    if q6p == "Q":
                        exit()
            print("++++++++++++\n")
            widthf = "Data Width of (Bytes) "+fxy+" : "
            while True:
                width = input(widthf)
                questionwidth = " "+width+" entered for \
                                the Data Width of "+fxy+"\n"
                print(questionwidth)
                q7p = input("is this correct?: (Y to proceed , N to \
                       re-enter the Data Width or Q to Quit) ")
                q7p = q7p.upper()
                if q7p == "Y" or q7p == "Q":
                    if q7p == "Y":
                        break
                    if q7p == "Q":
                        exit()
            revf = float(rev)
            revf = revf+1
            revfs = str(revf)
            row.append(revfs)
            row.append(desc)
            row.append(unit)
            row.append(scale)
            row.append(refer)
            row.append(width)
            row2.append(row)
            master_l[fxy] = row2
        return(master_l)
# following applies to Table D
    else:
        y = input("No of D Sequences to be created: ")
        y2 = int(y)
        for x in range(0, y2):
            ntx = "Title of the new Sequence : "
            ntx2 = raw_input(ntx)
            print "ntx2 ",ntx2,"\n"  
            nt = str(ntx2)
            fxy = input("Enter the sequence identifier (FXY):  ")
            qa = "No of descriptors in D sequence "+str(fxy)+" :  "
            nd = input(qa)
            fxy2 = fxy
            row = []
            ndi = int(nd)
            row.append(str(nt))
            row.append(nd)
            for z in range(0, ndi):
                zn = z+1
                zns = str(zn)
                zd = "Enter element no "+zns+" (FXY) :"
                while True:
                    seqd = input(zd)
                    questionwidth = " "+str(seqd)+" entered as desciptor No \
                    "+str(zns)+" in sequence "+str(fxy)+"\n"
                    print(questionwidth)
                    di = raw_input("is this correct?: (Y to proceed , \
                    N to re-enter the Data Width or Q to Quit) ")
                    dirp = str(di.upper())
                    dlen = len(str(seqd))
                    if dirp == "Y" or dirp == "Q":
                        if dirp == "Y":
                            if dlen != 6:
                                print("Descriptor has \
                                       incorrect length, re-enter\n")
                            else:
                                row.append(str(seqd))
                                break
                        if dirp == "Q":
                            exit()
            master_l[fxy2] = row
            print "master_l[fxy2] ",fxy2," ",master_l[fxy2],"\n"
        return master_l
#
###############################################################################
# subroutine name: final_dictb
#
# purpose:         Merge the Global (extracted from the WMO xml file)
#                  and the current operational versions of BUFR table B
#
# input:           masterl      -   Dict of lists for the current operational
#                                   TableB (formatted as nested lists)
#                                   example -
#
#                                   dict[010114][[rev(1),Descr,unit,scale,
#                                   ref val,data width],[rev(2),Descr,unit
#                                   ,scale,ref val,data width]]
#
#                  masterg      -   Dict of lists for version WMO TableB
#
# output:          fdict        -   Dict of lists. Each version of a
#                                   descriptor is nested within the
#                                   list as a seperate list.
#
#                                   dict[010114][[rev(1),Descr,unit,scale
#                                   ,ref val,data width],[rev(2),Descr
#                                   ,unit,scale,ref val,data width]]
#
##############################################################################


def final_dictb(masterl, masterg, revm):
    fdict = {}
    tlist = []
    revmc = str(revm)
    for key in list(masterl.keys()):
        flist = []
        listl = []
        listg = []
        listtmp = []
        listl = masterl[key]
        sl = len(listl)
        listl2 = listl
        if sl > 1:
            for z in range(0, sl):
                if z < sl-1:
                    af1 = listl2[z]
                    af2 = listl2[z+1]
                    aflist = list(set(af1[1:]) - set(af2[y][1:]))
                    if len(aflist) == 8:
                        listl2.pop(z+1)
            listl = listl2
#
        if key in masterg:
            listg = masterg[key][1:]
            gstring = listg
            for q in range(0, 8):
                gul = gstring[q]
                gul2 = gul.upper()
                gstring[q] = gul2
            listg = gstring
            sl2 = len(listl)
            listg.insert(0, revmc)
            listg1 = listg[3:6]+listg[7:]
            listl2 = listl[0][3:6]+listl[0][7:]
            rep1 = []
            rep2 = []
            for xh in listg1:
                df = xh.replace(' ', '')
                rep1.append(df)
            listg1 = rep1
            for xg in listl2:
                dg = xg.replace(' ', '')
                rep2.append(dg)
            listl2 = rep2
            flist = list(set(listg1) - set(listl2))
            flen = len(flist)
            if flen > 0:
                listl.insert(0, listg)
            fdict[key] = listl
        else:
            for y in range(0, sl):
                listm = listl[y]
                lenlm = len(listm)
                for z in range(0, lenlm):
                    mstring = listm[z]
                    mul = mstring.upper()
                    listm[z] = mul
                listl[y] = listm
            fdict[key] = (listl)
#
############################################################
#
    for key in list(masterg.keys()):
        gtemp = []
        gtempx = []
        if key not in fdict:
            gtemp = masterg[key]
            for x in range(1, 7):
                gstring = gtemp[x]
                gul = gstring.upper()
                gtemp[x] = gul
            gtempx.append(gtemp)
            gtempx[0][0] = revmc
            fdict[key] = gtempx
#
############################################################
#
    for key in sorted(fdict.keys()):
        temp = []
        temp = fdict[key]
        if temp[0][0] == '':
            temp[0][0] = '0'
        temp.sort(key=itemgetter(0), reverse=True)
        fdict[key] = temp
#
    for key in sorted(fdict.keys()):
        temprev = []
        temprev = fdict[key]
        templ = len(temprev)
        if templ > 1:
            for y in range(0, templ-1):
                tiprev = temprev[y][0]
                tiprev = "+"+tiprev
                temprev[y][0] = tiprev
            fdict[key] = temprev
    return fdict
#
##############################################################################
# subroutine name: final_dictd
#
# purpose:         Merge the Global (extracted from the WMO txt file) and
#                  the current operational versions of BUFR table D
#
# input:           masterld      -   Dict of lists for the current
#                                    operational Tabled (formatted as
#                                    nested lists)
#
#                  mastergd      -   Dict of lists for version WMO Tabled
#
# output:          fdictd        -   Dict of lists.
#
###############################################################################


def final_dictd(masterld, mastergd):
    fdictd = {}
    
    for key in list(masterld.keys()):
        if key in mastergd:
            fdictd[key] = mastergd[key]
        else:
            fdictd[str(key)] = masterld[key]   
#
    for key in list(mastergd.keys()): 
        if key not in fdictd:
            fdictd[str(key)] = mastergd[key]
    return fdictd
#
###############################################################################
# subroutine name: getdatb
#
# purpose:         read in the Global version of TableB (extracted
#                  from the WMO txt file)
#
# input:           tablenb       -   TableB txt
#
# output:          Tablebout     -   Dict of converted values.
#                  version       -   WMO version of the tables
#
##############################################################################


def getDatb(tablenb):
    vb = open(tablenb, 'r')
    vname = tablenb.split("_")
    vf = vname[1]
    vdot = ".00"
    wversion = vf+vdot
    cline = 0
    tablebout = {}
    tlist = []
    next(vb)
    for line in vb:
        tlist = []
        stringtb = line.replace(";", " ")
        stringtb = stringtb.replace("\",\"", ";")
        stringtb = stringtb.replace("\",,\"", ";;")
        stringtb = stringtb.replace("\",,,\"", ";;;")
        stringtb = stringtb.replace("\",,,,\"", ";;;;")
        stringtb = stringtb.replace(",\"", ";")
        stringtb = stringtb.replace("\",", ";")
        stringtb = stringtb.replace("\"", "")
        stringtb = stringtb.replace(",", " ")
        stringbf = stringtb.split(";")
        FXY = stringbf[3]
        desc = stringbf[4]
        units = stringbf[6]
        scale = stringbf[7]
        refs = stringbf[8]
        refs2 = refs.strip()
        dwidth = stringbf[9]
        cunits = stringbf[10]
        cscale = stringbf[11]
        cwidth = stringbf[12]
        tlist.append(wversion)
        tlist.append(desc)
        tlist.append(units)
        tlist.append(scale)
        tlist.append(refs2)
        tlist.append(dwidth)
        tlist.append(cunits)
        tlist.append(cscale)
        tlist.append(cwidth)
        tablebout[FXY] = tlist
#
    return tablebout, wversion
#
###############################################################################
# subroutine name: getdatd
#
# purpose:         read in the Global version of Tabled (extracted
#                  from the WMO txt file)
#
# input:           tablend       -   Tabled txt
#
# output:          Tabled        -   Dict of converted values.
#                  version       -   WMO version of the tables
#
############################################################################


def getDatd(Tablend):
    rowd = []
    rowf = []
    tabled = {}
    tabledt = {}
    count = 0
    version = 0
    cline = 0
    vn = open(Tablend, 'r')
    vers = Tablend.split("_")
    version = vers[1]+"."+vers[2]+"00"
    next(vn)
    for line in vn:
        stringtf = line.replace(";", " ")
        stringtf = stringtf.replace("\",\"", ";")
        stringtf = stringtf.replace("\",,\"", ";;")
        stringtf = stringtf.replace("\",,,\"", ";;;")
        stringtf = stringtf.replace(",\"", ";")
        stringtf = stringtf.replace("\",", ";")
        stringtf = stringtf.replace("\"", "")
        stringtf = stringtf.replace(",", " ")
        stringt = stringtf.split(";")
        keystr = stringt[3]
        keyst = keystr.lstrip(" ")
        keyst2 = keyst.lstrip(" ")
        keyst = keyst2
        ints = len(keystr)
        comp = stringt[6]
        if keyst not in list(tabled.keys()):
            seqt = stringt[4]
            rowd = []
            rowd.insert(0, seqt)
            rowd.append(comp)
            tabled[keyst] = rowd
        else:
            rowd = tabled[keyst]
            rowd.append(comp)
            tabled[keyst] = rowd
    for key in list(tabled.keys()):
        templ = tabled[key]
        count = len(templ[1:])
        templ.insert(1, count)
        tabled[key] = templ
    return tabled, version
#
###############################################################################
# subroutine name: output_b
#
# purpose:         output BUFR TableB
#
# input:           f_dict     -  dict of lists, containg all versions of
#                                the descriptor
#                  O_ver      - Version no of the WMO xml data
#                  N_Ver      - UKMO Ver no of the operational BUFR tableB
#
# output:          BUFR Table B - BUFR_TableB_YY-MM-DD_HH:MM:SS.txt
#
#
###############################################################################


def output_b(f_dict, UKMO_ver, WMO_ver, build_dir):
    dummy = "xxxxxxxxx               0   0  O\n"
    pwd = os.getcwd()
    i = datetime.now()
    sxt = i.strftime('%d %b %Y')
    sxtb = "("+sxt+")"
    xt = i.strftime('%Y/%m/%d %H:%M:%S')
    xt = i.strftime('%d/%m/%Y %H:%M:%S')
    xtfa = xt[0:6]
    xtfb = xt[8:]
    xtf = xtfa+xtfb
    xp = i.strftime('%Y-%m-%d_%H:%M:%S')
    filename = build_dir+"/output/BUFR_TableB_"+xp+".txt"
    bf = open(filename, 'w')
    mlen = 0
    maxl = 0
    n = 0
    F_WMO = float(WMO_ver)
    I_WMO = int(F_WMO)
    S_WMO = str(I_WMO)
    F_UKMO = float(UKMO_ver)
    F_UKMO = F_UKMO+1
    F_UKMO = int(F_UKMO)
    S_UKMO = str(F_UKMO)
    str1 = "WMO Version "
    fstr1 = str1 + S_WMO+".0.0"
    fstr2 = str1 + S_WMO
    xt = xt
    O_xter = str(xt)
#
    for key in sorted(f_dict.keys()):
        y = len(f_dict[key])
        if y > n:
            n = y
    vstring1 = '{message:{fill}{align}{width}}'.\
               format(message=fstr1, fill=' ', align='>', width=21)
    vstring2 = '{message:{fill}{align}{width}}'.\
               format(message=sxtb, fill=' ', align='>', width=17)
    vstring3 = '{message:{fill}{align}{width}}'.\
               format(message=fstr2, fill=' ', align='>', width=38)
#
    bf.write(('%s') % (vstring1))
    bf.write(('%s') % (vstring2))
    bf.write(('%s\n') % (vstring3))
    for x in range(0, n):
        for key in sorted(f_dict.keys()):
            lenf = len(f_dict[key])
            if x < lenf:
                string1 = f_dict[key][x][1]
                string1 = string1.strip()
                lenit = len(f_dict[key][x])
                if (len(string1)) > 60:
                    string1 = string1[0:59]
                key.strip()
                fkey = '{message:{fill}{align}{width}}'.\
                    format(message=key, fill=' ', align='<', width=7)
                bf.write(('%s') % (fkey))
#
                string1.upper()
                string1.strip()
                string1.lstrip()
                fstring1 = '{message:{fill}{align}{width}}'.\
                    format(message=string1, fill=' ', align='<', width=65)
                bf.write(('%s') % (fstring1))
#
                string3 = f_dict[key][x][0]
                string3 = string3.strip()
                if string3.find("+") == -1 and len(string3) < 6:
                    string3a = string3 + "0"
                    string3 = string3a
                elif string3.find("+") > -1 and len(string3) < 7:
                    string3a = string3 + "0"
                    string3 = string3a
                fstring3 = '{message:{fill}{align}{width}}'.\
                    format(message=string3, fill=' ', align='>', width=8)
                bf.write(('%s\n') % (fstring3))
#
                stringu = f_dict[key][x][2]
                stringu = stringu.lstrip()
                if len(stringu) > 26:
                    stringu2 = stringu[0:25]
                    stringu = stringu2
                    stringt1 = stringu.find("CODE TABLE")
                    stringt2 = stringu.find("code table")
                    stringt3 = stringu.find("COMMON CODE TABLE")
                    if (stringt1 >= 0 or stringt2 >= 0) and stringt3 == -1:
                        stringu = "CODE TABLE"
                stringu = " " + stringu
                fstringu = '{message:{fill}{align}{width}}'.format(
                    message=stringu, fill=' ', align='<', width=23)
                bf.write(('%s') % (fstringu))
#
                stringsc = f_dict[key][x][3]
                fstringsc = '{message:{fill}{align}{width}}'.format(
                    message=stringsc, fill=' ', align='>', width=6)
                bf.write(('%s') % (fstringsc))
#
                string4 = f_dict[key][x][4]
                string4.strip()
                fstringrf = '{message:{fill}{align}{width}}'.format(
                    message=string4, fill=' ', align='>', width=11)
                bf.write(('%s') % (fstringrf))
#
                string5 = f_dict[key][x][5]
                fstringw = '{message:{fill}{align}{width}}'.format(
                    message=string5, fill=' ', align='>', width=3)
                if lenit > 6:
                    bf.write(('%s') % (fstringw))
#
                    stringcrex1 = f_dict[key][x][6]
                    stringcrex1 = " "+stringcrex1
                    fstringu = '{message:{fill}{align}{width}}'.format(
                        message=stringcrex1, fill=' ', align='<', width=26)
#
                    stringcrex2 = f_dict[key][x][7]
                    fstrings = '{message:{fill}{align}{width}}'.format(
                        message=stringcrex2, fill=' ', align='>', width=3)
                    bf.write(('%s') % (fstrings))
#
                    stringcrex3 = f_dict[key][x][8]
                    fstringw = '{message:{fill}{align}{width}}'.format(
                        message=stringcrex3, fill=' ', align='>', width=3)
                    bf.write(('%s') % (fstringw))
                    stringcrex4 = " O"
                    fstringo = '{message:{fill}{align}{width}}'.format(
                        message=stringcrex4, fill=' ', align='>', width=2)
                    bf.write(('%s\n') % (fstringo))
                else:
                    bf.write(('%s\n') % (fstringw))
#
    print("===============================================================\n") 
    print "\n" 
    print " BUFR TableB output - ", filename, "\n" 
    print("\n")
#
###########################################################################
# subroutine name: output_d
#
# purpose:         output BUFR TableD
#
# input:           fdictd     -  dict of tableD entries (tip rev only)
#                  revn       - wmo version no of the new table
#                  wmo_rev    - wmo version non of the table
#
# output:          BUFR Table - BUFR_TableD_YY-MM-DD_HH:MM:SS.txt
#
#
############################################################################


def output_d(fdictd, revn, wmo_rev, build_dir):
    i = datetime.now()
    sxt = i.strftime('%d %b %Y')
    sxtb = "("+sxt+")"
    xt = i.strftime('%Y-%m-%d_%H:%M:%S')
    pwd = os.getcwd()
    fdict_out = build_dir+"/output/BUFR_TableD_"+xt+".txt"
    rp = revn.find(".")
    renip = revn[:rp]
    reni = int(renip)
    reni = reni+1
    renc = str(reni)
#
    str1 = "WMO Version "
    fstr1 = str1+renc+".0.0"
#
    fdo = open(fdict_out, 'w')
    vstring1 = '{message:{fill}{align}{width}}'.format(
            message=fstr1, fill=' ', align='>', width=22)
    vstring2 = '{message:{fill}{align}{width}}'.format(
            message=sxtb, fill=' ', align='>', width=16)
    fdo.write(('%s') % (vstring1))
    fdo.write(('%s\n') % (vstring2))
    fdo.write(('%s\n') % (" "))
#   
    tdref2 = []
    for keyz in fdictd.keys():
        tdref2.append(str(keyz))
    tdref2.sort()
#
#
    for key in sorted(fdictd.keys()): 
        templ = []
        templ = fdictd[key][2:]
        rl = len(templ)
        desct = fdictd[key][0]
        desctsf = "          "+fdictd[key][0]
        descts = desct.strip(" ")
        if len(descts) == 0:
            desct = "NIL"
        desc_c = fdictd[key][1]
        if isinstance(desc_c, str):
            desc_ci = int(desc_c)
            desc_c = desc_ci
        if desc_c > 99:
            kwidth = 6
            cwidth = 4
        elif desc_c >= 10 and desc_c <= 99:
            kwidth = 7
            cwidth = 3
        elif desc_c < 10:
            kwidth = 8
            cwidth = 2
#
        Dkey = '{message:{fill}{align}{width}}'.format(
                           message=key, fill=' ', align='<', width=kwidth)
        fcount = '{message:{fill}{align}{width}}'.format(
                           message=desc_c, fill=' ', align='<', width=cwidth)
        fdesct = '{message:{fill}{align}{width}}'.format(
                           message=desctsf, fill=' ', align='<', width=70)
        if desct != "NIL":
            fdo.write(('%s\n') % (fdesct))
        fdo.write(('%s') % (Dkey))
        fdo.write(('%s') % (fcount))
        for x in range(0, rl):
            descd_out = templ[x]
            Descd = '{message:{fill}{align}{width}}'.format(
                               message=descd_out, fill=' ', align='<', width=7)
            xrem = x % 10
            if xrem == 0 and x != 0:
                fdo.write(('%s\n') % (" "))
                fdo.write(('%s') % (Descd))
            else:
                fdo.write(('%s') % (Descd))
        fdo.write(('%s\n') % ("  "))
        fdo.write(('%s\n') % ("  "))
    fdo.close()
    print("=================================================================\n")
    print("\n")
    print " BUFR TableD output - ", fdict_out, "\n" 
    print("\n")
#
#
###############################################################################
# subroutine name: comp_versions
#
# purpose:         compare current and new versions of BUFR TableB or TableD D.
#                  Output revision information of descriptors or sequences
#                  deprecated and/or added
#
# input:           nvers      - new version of the table
#                  overs      - old version of the table
#                  table      - B or D indicator
#                  wmo_rev    - wmo version no
#                  loc_rev    - local (metDB) version no
#                  bdir       - Build/output directory
#
# output:          Revision information  -
#
#                  TableB_Revision_Information_YY-MM-DD_HH:MM:SS.txt
#
#                  TableD_Revision_Information_YY-MM-DD_HH:MM:SS.txt
#
#
###############################################################################


def comp_versions(nvers, overs, table, wmo_rev, loc_rev, build_dir):
    ic = datetime.now()
    xc = ic.strftime('%Y-%m-%d_%H:%M:%S')
    pwd = os.getcwd()
    c_dict = {}
    m_dict = {}
    if table == "B":
        fc_out = build_dir+"/output/TableB_Revision_Information"+xc+".txt"
        fco = open(fc_out, 'w')
 #       for key in list(nvers.keys()):
        for key in nvers.keys():
            if key in overs:
                lenov = len(overs[key])
                tlstb = []
                for z in range(0, lenov):
                    tlsta = []
                    tlsta = overs[key][z][3:6]                      
                    tlst1a = [x.strip(' ') for x in tlsta]                   
                    tlstb.append(tlst1a)
                if len(tlstb) > 1:
                    nlist = []
                    nlist2 = [] 
                    nlist3 = []  
                    for xp2 in range(0,len(tlstb)):
                        nlist.append(tlstb[xp2][0])
                        nlist2.append(tlstb[xp2][1])     
                        nlist3.append(tlstb[xp2][2]) 
                    tfl = set(nlist)
                    tfx = list(tfl)
                    tfl2 = set(nlist2)
                    tfx2 = list(tfl2)
                    tfl3 = set(nlist3)
                    tfx3 = list(tfl3)
                    tfd = len(tfx)
                    tfd2 = len(tfx2)
                    tfd3 = len(tfx3)
                    if tfd > 1 or tfd2 > 1 or tfd3 > 1:  
                        c_dict[key] = overs[key]         
            else:
                m_dict[key] = nvers[key]
#
        spacer = " "
        Bspt = '{message:{fill}{align}{width}}'.format(
                              message=spacer, fill=' ', align='<', width=65)
        vspacer = "|"
        Bvspac = '{message:{fill}{align}{width}}'.format(
                              message=vspacer, fill=' ', align='<', width=4)
#        bdiv = "++++++++++++++++++++++++++++++++++++++++"
        Btdiv = '{message:{fill}{align}{width}}'.format(
                              message='+', fill='+', align='<', width=65)
        title = "Alterations to existing descriptors "
        Btitlet = '{message:{fill}{align}{width}}'.format(
                              message=title, fill=' ', align='<', width=65)
        rev = "Revision"
        Brevt = '{message:{fill}{align}{width}}'.format(
                              message=rev, fill=' ', align='<', width=17)
        FXY = "FXY"
        BFXYt = '{message:{fill}{align}{width}}'.format(
                              message=FXY, fill=' ', align='<', width=16)
        unit = "Unit"
        Bunitt = '{message:{fill}{align}{width}}'.format(
                              message=unit, fill=' ', align='<', width=18)
        scale = "Scale"
        Bscalet = '{message:{fill}{align}{width}}'.format(
                              message=scale, fill=' ', align='<', width=13)
        ref = "Reference"
        Breft = '{message:{fill}{align}{width}}'.format(
                              message=ref, fill=' ', align='<', width=14)
        dw = "Data Width"
        Bdw = '{message:{fill}{align}{width}}'.format(
                              message=dw, fill=' ', align='<', width=20)
        desc = "Description"
        Bdesct = '{message:{fill}{align}{width}}'.format(
                              message=desc, fill=' ', align='<', width=33)
        endt = "Comparison run Date / Time -"+xc
        Bendt = '{message:{fill}{align}{width}}'.format(
                              message=endt, fill=' ', align='<', width=30)
        output = "Descriptors introduced in WMO version " + wmo_rev + \
                 " of BUFR Table B"
        Bintro = '{message:{fill}{align}{width}}'.format(
                              message=output, fill=' ', align='<', width=40)
        Bspac = '{message:{fill}{align}{width}}'.format(
                              message=" ", fill=' ', align='<', width=11)
        flist = [11, 30, 16, 10, 10, 10]
#
        if len(c_dict) > 0:
            fco.write(('%s\n') % (Btitlet))
            fco.write(('%s\n') % (Bspt))
            fco.write(('%s') % (BFXYt))
            fco.write(('%s') % (Brevt))
            fco.write(('%s') % (Bdesct))
            fco.write(('%s') % (Bunitt))
            fco.write(('%s') % (Bscalet))
            fco.write(('%s') % (Breft))
            fco.write(('%s\n') % (Bdw))
            for key in sorted(c_dict):
                CFXY = '{message:{fill}{align}{width}}'.format(
                         message=key, fill=' ', align='<', width=11)
                templ = c_dict[key]
                fco.write(('%s\n') % (" "))
                fco.write(('%s') % (CFXY))
                fco.write(('%s') % (Bvspac))
                tl = len(templ)
                for x in range(0, tl):
                    plist = templ[x]
                    for y in range(0, 6):
                        fam = flist[y]
                        op_val = plist[y]
                        if y == 1:
                            if len(op_val) > 24:
                                op_val = op_val[0:23]
                        if y == 0:
                            if op_val.find("+") > -1:
                                opval2 = op_val[1:]
                                op_val = opval2
                        if y == 5:
                            Bopv = '{message:{fill}{align}{width}}'.format(
                                     message=op_val, fill=' ', align='<',
                                     width=fam)
                            fco.write(('%s\n') % (Bopv))
                        else:
                            if x > 0 and y == 0:
                                Bopv = '{message:{fill}{align}{width}}'.format(
                                          message=op_val, fill=' ', align='<',
                                          width=fam)
                                fco.write(('%s') % (Bspac))
                                fco.write(('%s') % (Bvspac))
                                fco.write(('%s') % (Bopv))
                                fco.write(('%s') % (Bvspac))
                            else:
                                Bopv = '{message:{fill}{align}{width}}'.format(
                                         message=op_val, fill=' ', align='<',
                                         width=fam)
                                fco.write(('%s') % (Bopv))
                                fco.write(('%s') % (Bvspac))
        fco.write(('%s\n') % (Bspac))
        fco.write(('%s\n') % (Bspt))
#
#        if any(m_dict):
        if len(m_dict) > 0:
            fco.write(('%s') % (Bintro))
            fco.write(('%s\n') % (Bspt))
            fco.write(('%s') % (BFXYt))
            fco.write(('%s') % (Brevt))
            fco.write(('%s') % (Bdesct))
            fco.write(('%s') % (Bunitt))
            fco.write(('%s') % (Bscalet))
            fco.write(('%s') % (Breft))
            fco.write(('%s\n') % (Bdw))
            for key in sorted(m_dict):
                B2key = '{message:{fill}{align}{width}}'.format(
                            message=key, fill=' ', align='<', width=13)
                fco.write(('%s') % (B2key))
                fco.write(('%s') % (Bvspac))
                for y in range(0, 6):
                    fam = flist[y]
                    op_val2 = m_dict[key][y]
                    if y == 1:
                        if len(op_val2) > 24:
                            op_val2 = op_val2[0:23]
                    B2opv = '{message:{fill}{align}{width}}'.format(
                              message=op_val2, fill=' ', align='<', width=fam)
                    if y == 5:
                        fco.write(('%s\n') % (B2opv))
                    else:
                        fco.write(('%s') % (B2opv))
                        fco.write(('%s') % (Bvspac))

        

        print("\n")
        print "Table B Revision output -", fc_out, "\n"
        print("\n")
        print('======================================================='\
              '======================================================='\
              '======================\n')
#
#####################################################################

    elif table == "D":
        ic = datetime.now()
        xc = ic.strftime('%Y-%m-%d_%H:%M:%S')
        pwd = os.getcwd()
        c_dict = {}
        m_dict = {}
        fc_out = build_dir+"/output/TableD_Comparison"+xc+".txt"
        fco = open(fc_out, 'w')
        seq_dep_new = {}
        seq_add_new = {}
        dep_old = {}
        add_new = {}
        list_old = []
        list_new = []
        flistn = []
        flisto = []
        for key in list(nvers.keys()):
            flistn = []
            flisto = []
            key2 = int(key[3:])
            intl = len(key)
            if key in list(overs.keys()) and key2 < 192:
                list_new = overs[key][2:]
                list_old = nvers[key][2:]
                ast = set(list_new)
                bst = set(list_old)
                edep = bst.difference(ast)
                nadd = ast.difference(bst)
                intnew = list(nadd)
                intold = list(edep)
                if len(intnew) > 0:
                    add_new[key] = intnew
                if len(intold) > 0:
                    dep_old[key] = intold
            elif key not in list(overs.keys()) and key2 < 192:
                list_new = []
                list_new = nvers[key]
                seq_add_new[key] = list_new
        for key in overs:
            key2 = int(key[3:])
            if key not in nvers and key2 < 192:
                list_old = []
                list_old = overs[key]
                seq_dep_new[key] = list_old
        titlem = ' FXY   |                   Descriptors / Sequences     '
        titlem2 = '       |  '
        spacer = '             Descriptors added to existing D sequences' \
                 ' in the new version of Table D'
        spacer1 = '                D Sequences added in the new version' \
                  'of Table D                '
        spacer2 = '             Descriptors deprecated from D sequences in' \
                  ' the new version of Table D'
        spacer3 = '             D Sequences deprecated from the new version' \
                  ' of Table D '

        nill = "              No Entries  "
        bout = '{message:{fill}{align}{width}}'.format(
                 message='+', fill='+', align='<', width=100)
        boutd = '{message:{fill}{align}{width}}'.format(
                 message='-', fill='-', align='<', width=100)
        bout0 = '{message:{fill}{align}{width}}'.format(
                 message=titlem, fill=' ', align='<', width=40)
        fdec = '{message:{fill}{align}{width}}'.format(
                 message=spacer, fill=' ', align='<', width=35)
        fdec1 = '{message:{fill}{align}{width}}'.format(
                  message=spacer1, fill=' ', align='<', width=35)
        fdec2 = '{message:{fill}{align}{width}}'.format(
                  message=spacer2, fill=' ', align='<', width=35)
        fdec3 = '{message:{fill}{align}{width}}'.format(
                  message=spacer3, fill=' ', align='<', width=35)
        null2 = '{message:{fill}{align}{width}}'.format(
                  message=nill, fill=' ', align='<', width=35)

#####################################################
        if any(seq_dep_new):
            fco.write(('%s\n') % (bout))
            fco.write(('%s\n') % ("  "))
            fco.write(('%s\n') % (fdec3))
            fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout0))
            fco.write(('%s\n') % (boutd))
            for key in list(seq_dep_new.keys()):
                olist = []
                olist = seq_dep_new[key]
                title = seq_dep_new[key][0]
                leno = len(olist)
                Dkey = '{message:{fill}{align}{width}}'.format(
                         message=key, fill=' ', align='<', width=8)
                sp3 = '{message:{fill}{align}{width}}'.format(
                        message='|', fill=' ', align='>', width=9)
                sp2 = '{message:{fill}{align}{width}}'.format(
                     message='| ', fill=' ', align='<', width=1)
                titleo = '{message:{fill}{align}{width}}'.format(
                         message=title, fill=' ', align='<', width=22)
                fco.write(('%s') % (sp3))
                fco.write(('%s\n') % (titleo))
                fco.write(('%s') % (Dkey))
                fco.write(('%s') % (sp2))

                for x in range(2, leno):
                    descb_out = olist[x]
                    Descb = '{message:{fill}{align}{width}}'.format(
                               message=descb_out, fill=' ', align='<', width=8)
                    sp1 = '{message:{fill}{align}{width}}'.format(
                           message=' ', fill=' ', align='<', width=8)
                    xrem = x % 10
                    if xrem == 0 and x != 0:
                        fco.write(('%s\n') % (""))
                        fco.write(('%s') % (sp1))
                        fco.write(('%s') % (sp2))
                        fco.write(('%s') % (Descb))
                    else:
                        fco.write(('%s') % (Descb))
                fco.write(('%s\n') % (""))
                fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout))
###########################################################################
        if any(seq_add_new):
            fco.write(('%s\n') % ("  "))
            fco.write(('%s\n') % (fdec1))
            fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout0))
            fco.write(('%s\n') % (boutd))
            for key in list(seq_add_new.keys()):
                olist = []
                otlist = seq_add_new[key][1:]
                title = seq_add_new[key][0]
                lenot = len(otlist)
                Dkey = '{message:{fill}{align}{width}}'.format(
                         message=key, fill=' ', align='<', width=8)
                sp2 = '{message:{fill}{align}{width}}'.format(
                       message='| ', fill=' ', align='<', width=1)
                if title == "NIL":
                    title = " "
                Dt = '{message:{fill}{align}{width}}'.format(
                       message=title, fill=' ', align='<', width=60)
                fco.write(('%s') % (Dkey))
                fco.write(('%s') % (sp2))
                for x in range(2, lenot):
                    desct_out = otlist[x]
                    Descbt = '{message:{fill}{align}{width}}'.format(
                               message=desct_out, fill=' ', align='<', width=8)
                    sp1 = '{message:{fill}{align}{width}}'.format(
                            message=' ', fill=' ', align='<', width=8)
                    xrem = x % 10
                    if xrem == 0 and x != 0:
                        fco.write(('%s\n') % (" "))
                        fco.write(('%s') % (sp1))
                        fco.write(('%s') % (sp2))
                        fco.write(('%s') % (Descbt))
                    else:
                        fco.write(('%s') % (Descbt))
                fco.write(('%s\n') % ("  "))
                fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout))
###########################################################################
        if any(dep_old):
            fco.write(('%s\n') % ("  "))
            fco.write(('%s\n') % (fdec2))
            fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout0))
            fco.write(('%s\n') % (boutd))
            for key in list(dep_old.keys()):
                nlist = []
                nlist = dep_old[key]
                lenn = len(nlist)
                Dkeyo = '{message:{fill}{align}{width}}'.format(
                          message=key, fill=' ', align='<', width=8)
                sp2 = '{message:{fill}{align}{width}}'.format(
                        message='| ', fill=' ', align='<', width=1)
                fco.write(('%s') % (Dkeyo))
                fco.write(('%s') % (sp2))
                for x in range(0, lenn):
                    descb_out = nlist[x]
                    Descot = '{message:{fill}{align}{width}}'.format(
                               message=descb_out, fill=' ', align='<', width=8)
                    sp1 = '{message:{fill}{align}{width}}'.format(
                            message=' ', fill=' ', align='<', width=8)
                    xrem = x % 10
                    if xrem == 0 and x != 0:
                        fco.write(('%s\n') % (" "))
                        fco.write(('%s') % (sp1))
                        fco.write(('%s') % (sp2))
                        fco.write(('%s') % (Descot))
                    else:
                        fco.write(('%s') % (Descot))
                fco.write(('%s\n') % ("  "))
                fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout))
#############################################################################
        if any(add_new):
            fco.write(('%s\n') % ("  "))
            fco.write(('%s\n') % (fdec))
            fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout0))
            fco.write(('%s\n') % (boutd))
            for key in list(add_new.keys()):
                ntlist = []
                ntlist = add_new[key]
                title = add_new[key][0]
                leno = len(ntlist)
                Dkey = '{message:{fill}{align}{width}}'.format(
                         message=key, fill=' ', align='<', width=8)
                if title == "NIL":
                    title = " "
                Dt = '{message:{fill}{align}{width}}'.format(
                       message=title, fill=' ', align='<', width=60)
                fco.write(('%s') % (Dkey))
                fco.write(('%s') % (sp2))
                for x in range(0, leno):
                    desct_out = ntlist[x]
                    Descott = '{message:{fill}{align}{width}}'.format(
                               message=desct_out, fill=' ', align='<', width=8)
                    xremt = x % 10
                    if xremt == 0 and x != 0:
                        fco.write(('%s\n') % (" "))
                        fco.write(('%s') % (sp1))
                        fco.write(('%s') % (sp2))
                        fco.write(('%s') % (Descott))
                    else:
                        fco.write(('%s') % (Descott))
                fco.write(('%s\n') % ("  "))
                fco.write(('%s\n') % (boutd))
            fco.write(('%s\n') % (bout))
###############################################################################
        print("***************\n")
        print("\n")
        fcm = "BUFR Table D Comparison output - " + fc_out + "\n"
        print(fcm)
        print("\n")
        print("***************\n")
###############################################################################
# subroutine name: read_in_tableb
#
# purpose:         Read in the current (operational) tableB to a dict.
#
# input:           Tablename  -  BUFR TableB filename (.txt)
#
# output:          Master     - Dict of lists of the form -
#
#                 dict[010114][[rev(1),Descr,unit,scale,ref val,
#                              data width],[rev(2),Descr,unit,scale,ref val,
#                              data width]]
#
#                 rev_ukmo - local table revision information
#
#
#
###############################################################################


def read_in_tableb(tablename):
    rev = "WMO Version"
    second = 0
    drow = []
    master = {}
    local = {}
    with open(tablename, 'r') as fb:
        for line in fb:
            if rev not in line:
                test = line[0:6]
                if test.isdigit() and second == 0:
                    ident = str(line[0:6])
                    ident = ident.strip(' \t\n\r')
                    gident = int(ident[0:3])
                    lident = int(ident[3:6])
                    desc = line[7:61]
                    desc = desc.strip(' \t\n\r')
                    metrev = line[74:]
                    metrev2 = metrev.replace("+", "")
                    metrev = metrev2
                    metrev = metrev.strip(' \t\n\r')
                    second = 1
                elif second == 1:
                    drow = []
                    unit = line[1:13]
                    unit = unit.strip(' \t\n\r')
                    scale = line[26:29]
                    scale = scale.strip(' \t\n\r')
                    ref=line[29:40]
                    ref = ref.strip(' \t\n\r')
                    dw = line[40:43]
                    dw = dw.strip(' \t\n\r')
                    cunit = line[44:67]
                    cunit = cunit.strip(' \t\n\r')
                    cscale = line[70:73]
                    cscale = cscale.strip(' \t\n\r')
                    cdw = line[73:76]
                    cdw = cdw.strip(' \t\n\r')
                    drow.append(metrev)
                    drow.append(desc)
                    drow.append(unit)
                    drow.append(scale)
                    drow.append(ref)
                    drow.append(dw)
                    drow.append(cunit)
                    drow.append(cscale)
                    drow.append(cdw)
                    if ident in master:
                        drowt = master[ident]
                        drowt.append(drow)
                        xl = len(master[ident])
                        if xl > 1:
                            drowt.sort(key=itemgetter(0))
                            master[ident] = drowt
                    else:
                        drowt = []
                        drowt.append(drow)
                        drow = list(drowt)
                        master[ident] = drow
                    second = 0
            else:
                rev_line = line[0:]
                point = rev_line.find(rev)
                rev_ukmo = rev_line[point+12:point+16]
    for key in sorted(master.keys()):
        tempm = []
        tempm = master[key]
        tempm.sort(key=itemgetter(0), reverse=True)
        master[key] = tempm
    return master, rev_ukmo
#  note rev_ukmo - UKMO revision no
#############################################################################
# subroutine name: read_in_tabled
#
# purpose:         Read in the current (operational) BUFR
#                  TableD to a dict.
#
# input:           Tablename  -  BUFR TableD filename (.txt)
#
# output:          dseq      - Dict of lists
#
#
#############################################################################


def read_in_tableD(tableD):
    dseq = {}
    rev2 = ""
    dline = []
    n = 0
    nseq = 0
    fin = open(tableD, 'r')
    for line in fin:
        bc = line.rstrip(" ")
        bc2 = bc.lstrip(" ")
        bc3 = bc2.strip("\n")
        bc4 = bc3.strip("\r")
        lbc = len(bc4)
        if line.find('WMO Version') != -1:
            rev2 = line[15:19]
            rev2.strip()
        else:
            if lbc == 0:
                nseq = 0
                title = ""
            elif nseq == 0 and lbc > 0:
                if bool(re.search('[a-zA-Z]', line)):
                    title = ""
                    title = line
                    title.lstrip()
                    title.rstrip()
                    titlef = title.replace("(", "")
                    titlef2 = titlef.replace(")", "")
                    titlef2 = re.sub(r'\s+', ' ', titlef2).strip()
                    title = titlef2
                else:
                    temp_l = ' '.join(line.split())
                    templ = temp_l.split(" ")
                    if len(templ[0]) > 6:
                        key = templ[0][:6]
                        count = templ[0][6:]
                        dline = templ[1:]
                    else:
                        key = templ[0]
                        count = templ[1]
                        dline = templ[2:]
                    dline.insert(0, title)
                    dline.insert(1, count)
                    nseq = nseq+1
                    dseq[key] = dline
            elif lbc > 0 and nseq > 0:
                temp_l = re.sub(r'\s+', ' ', line).strip()
                templ = temp_l.split(" ")
                dline = dline+templ
                dseq[key] = dline
    return dseq, rev2
##############################################################################
# subroutine name: read_oldfig
#
# purpose:         Read in the current (operational)
#                  BUFR Codefigs to a dict.
#
# input:           Tablename  -  BUFR Codefigs (.txt)
#
# output:          fin_dict      - Dict of Codefigs
#                  rev_no        - Revision No of the current
#                                  version of codefigs
#
#
###############################################################################


def read_oldfig(file1):
    fin_dict = {}
    clist = []
    tlist = []
    with open(file1, 'r') as fin:
        for line in fin:
            if line.find("$Revision") >= 0:
                rev_no = int(line[12:14])
                line = next(fin)
                line = next(fin)
            line_test = line.strip()
            if line_test:
                if line.find("!") >= 0:
                    ex = line.find("!")
                    line = line[:ex-1]
                line = line.rstrip()
                linea = line.split()
                el = len(linea[0])
                elf = len(linea)
                if el == 6:
                    tlist = []
                    clist = []
                    key = linea[0]
                    ct_start = line.find(linea[1])
                    ct_title = line[ct_start:]
                    ct_title = ct_title.rstrip()
                    tlist.append(ct_title)
                    clist.append(tlist)
                else:
                    if elf == 1:
                        linea.append("-")
                    tlist = []
                    entry_no = linea[0]
                    ent_start = line.find(linea[1])
                    ent_det = line[ent_start:]
                    tlist.append(entry_no)
                    tlist.append(ent_det)
                    clist.append(tlist)
            else:
                fin_dict[key] = clist
    fin.close()
    return(fin_dict, rev_no)
#
###############################################################################
# subroutine name: read_newfig
#
# purpose:         Read in the WMO version of BUFR Codefigs to a dict.
#
# input:           Tablename  - WMO Codefigs (.txt)
#
# output:          fin_dict      - Dict of Codefigs
#                  rev_no        - Revision No of the current
#                                  version of codefigs
#
#
###############################################################################


def read_newfig(file2):
    ct_dict = {}
    ct_ent = []
    ct_holder = []
    with open(file2, 'r') as fin2:
        bline = next(fin2)
        for ct in fin2:
            ct = ct.split(",")
            test_list = [x for x in ct if x]
            if len(test_list) > 4:
                tab_no = ct[1]
                tab_title = ct[2]
                tab_ref = ct[3]
                tab_ent = ct[4]
                tab_ref2 = tab_ref.strip("\"")
                tab_title2 = tab_title.strip("\"")
                tab_ent2 = tab_ent.strip("\"")
                tab_no2 = tab_no.strip("\"")
                if tab_no2 in ct_dict:
                    ct_ent = []
                    ct_ent.append(tab_ref2)
                    ct_ent.append(tab_ent2)
                    ct_holder.append(ct_ent)
                    ct_dict[tab_no2] = ct_holder
                else:
                    ct_ent = []
                    ct_holder = []
                    ct_ent.append(tab_title2)
                    ct_holder.insert(0, ct_ent)
                    ct_ent = []
                    ct_ent.append(tab_ref2)
                    ct_ent.append(tab_ent2)
                    ct_holder.append(ct_ent)
                    ct_dict[tab_no2] = ct_holder
            else:
                tab_no = ct[1]
                tab_title = ct[2]
                tab_no2 = tab_no.strip("\"")
                tab_title2 = tab_title.strip("\"")
                if tab_no2 not in ct_dict:
                    ct_ent = []
                    ct_holder = []
                    tab_title3 = tab_title2+" Refer to common code table"
                    ct_ent.append(tab_title3)
                    ct_holder.append(ct_ent)
                    ct_dict[tab_no2] = ct_holder
    fin2.close()
    return(ct_dict)
#
###############################################################################
# subroutine name: mdict
#
# purpose:         merge WMO and MetDB versions of the BUFR Codefig tables
#
# input:           masterld     -  local version of the  codefig tables
#                  mastergd     -  WMO Version (New) version of the code
#                                  tables
#
# output:          fdictd       -  Merged data in a dictionary
#
#
###############################################################################


def mdict(masterld, mastergd):
    fdictd = {}
    for key in list(masterld.keys()):
        if key in mastergd:
            fdictd[key] = mastergd[key]
        else:
            fdictd[key] = masterld[key]
    for key in list(mastergd.keys()):
        if key not in fdictd:
            fdictd[key] = mastergd[key]
    return fdictd
#
############################################################################
# subroutine name: out_codefig
#
# purpose:         output BUFR Codefig tables
#
# input:           mfdict     -  Merged codefig tables
#
# output:          BUFR CodeFig Table - CodeFig_YY-MM-DD_HH:MM:SS.txt
#
#
############################################################################


def out_codefig(mfdictd, rno, build_dir):
    i = datetime.now()
    xt = i.strftime('%Y-%m-%d_%H:%M:%S')
    xp = i.strftime('%d/%m/%Y %H:%M:%S')
    pwd = os.getcwd()
    fdict_out = build_dir+"/output/CodeFig_"+xt+".txt"
#
    fdo = open(fdict_out, 'w')
    revno = rno+1
    revno2 = str(revno)
    tline = "  $Revision: "+revno2+"$Date:  "+xp+"$"
    tlineo = '{message:{fill}{align}{width}}'.format(
             message=tline, fill=' ', align='<', width=46)
    fdo.write(('%s\n') % (tlineo))
    fdo.write(('%s\n') % ("  "))
    for key in sorted(mfdictd):
        title = mfdictd[key][0][0]
        key2 = "  "+key
        fkey = '{message:{fill}{align}{width}}'.format(
                 message=key2, fill=' ', align='>', width=8)
        title = " "+title
        title = title.upper()
        ftitle = '{message:{fill}{align}{width}}'.format(
                   message=title, fill=' ', align='<', width=40)
        fdo.write(('%s') % (fkey))
        fdo.write(('%s\n') % (ftitle))
        arr = mfdictd[key][1:]
        lent = len(arr)
        for y in range(0, lent):
            cno = arr[y][0]
            entry = arr[y][1]
            entry = entry.upper()
            fcno = '{message:{fill}{align}{width}}'.format(
                     message=cno, fill=' ', align='<', width=4)
            fentry = '{message:{fill}{align}{width}}'.format(
                       message=entry, fill=' ', align='<', width=50)
            fdo.write(('%s') % (fcno))
            fdo.write(('%s\n') % (fentry))
    fdo.write(('%s\n') % ('  '))
    print("===============================================================\n")
    print("\n")
    print(" BUFR Codefig output - ", fdict_out, "\n")
    print("\n")
    print("===============================================================\n")
#
##############################################################################
# subroutine name: link_common_tables
#
# purpose:         Cross reference the entries in the merged data dictionary
#                  with the Common Code Tables
#
# input:           mfdict     -  Merged codefig tables
#
# output:          mdict_out  -  Merged codefig tables with entries from the
#                                common code tables.
#
#
##############################################################################


def link_common_tables(mfdict2, hdr):
    link = {}
    link["05"] = ["SATELLITE IDENTIFIER", "001007"]
    link["07"] = ["TRACKING TECHNIQUE/SYSTEM STATUS", "002014"]
    link["08"] = ["SATELLITE INSTRUMENTS ", "002019"]
    link["01"] = ["ORIGINATING/GENERATING CENTRE (SEE 001031)", "001033"]
    link["02"] = ["RADIOSONDE TYPE ", "002011"]
#########################################
    file_list = []
    cf_dict = {}
    psd = os.getcwd()
    psf = hdr+"/common_ct/"
    for filename in os.listdir(psf):
        file_list.append(filename)
    match = []
    master = []
    for key in list(link.keys()):
        tab = key
        s_pattern = "Common_C"+tab
        match = [s for s in file_list if s_pattern in s]
        master.append(match)
        mlen = len(master)
        master2 = []
        masterf = []
        for y in range(0, mlen):
            fname = master[y][0]
            fname_full = psf+fname
            fname_label = fname[8:10]
            if fname_label == "05":
                c1 = 2
                c2 = 4
            elif fname_label == "08":
                c1 = 1
                c2 = 4
            elif fname_label == "07":
                c1 = 2
                c2 = 3
            else:
                c1 = 3
                c2 = 4
            with open(fname_full, 'r') as fr:
                first_ent = []
                first_ent = link[fname_label][1]
                ftitle = link[fname_label][0]
                line = next(fr)
                master2 = []
#
                master2.append(ftitle)
                masterf.append(master2)
                master2 = []
#
                for line in fr:
                    list1 = []
                    sl = line.replace("\"", "")
                    sline = sl.split(",")
                    sline2 = [x for x in sline if x]
                    if len(sline2) <= 3:
                        line = next(fr)
                    else:
                        if fname_label == "08":
                            code_num = sline[c1]
                            code_desc = sline[c2]
                        else:
                            code_num = sline[c1]
                            code_desc = sline[c2]
                        list1.append(code_num)
                        list1.append(code_desc)
                        masterf.append(list1)
#
                out_key = link[fname_label][1]
                cf_dict[out_key] = masterf
                masterf = []
            fr.close()
    for key in list(cf_dict.keys()):
        key2 = key
        tlist = []
        if key2 in mfdict2:
            tlist2 = cf_dict[key2]
            mfdict2[key2] = tlist2
    return(mfdict2)
#
###############################################################################
# subroutine name: comp_code_dicts
#
# purpose:         Run comparison of the new and old codefig files
#
# input:           dict1     -  Current MetDB codefig tables
#                               (dictionary of lists)
#
#                  dict2     -  New Code Fig tables downloaded from
#                               the WMO Web Pages
#
# returns:          mdict_out  -  Merged codefig tables with entries
#                                 from the common code tables.
#
#
###############################################################################


def comp_code_dicts(dict1, dict2):
    list_old = []
    list_new = []
    diff_old = {}
    diff_new = {}
    for key in dict1:
        if key in dict2:
            list_new = dict2[key]
            list_old = dict1[key]
            diff_l = [x for x in list_old if x not in list_new]
            if len(diff_l) > 0:
                diff_old[key] = diff_l
            else:
                diff_old[key] = dict1[key]
#
    for key in dict2:
        if key in dict1:
            list_new = dict1[key]
            list_old = dict2[key]
            diff_l = [x for x in list_new if x not in list_old]
            if len(diff_l) > 0:
                diff_new[key] = diff_l
        else:
            diff_new[key] = dict2[key]
    return()
#
#############################################################################
# subroutine name: rep_d
#
# purpose:         Expand all D sequences, replacing nested D sequences
#                  with full listings of B Descriptors
#
# input:           odictn     -  Dict of D Sequences.
#
# output:          odictf  -   Dictionary containing expanded D sequence.
#
#
#############################################################################


def rep_d(odictn):
    odictf = {}
    for key in list(odictn.keys()):
        ilistd = []
        ilistd = odictn[key][2:]
        D_test = "True"
        while D_test:
            restart = False
            n = 0
            xylen = len(ilistd)
            for i in range(0, xylen):
                n = i
                ts = ilistd[i][0]
                tsf = ilistd[i]
                if ts == "3":
                    tsfi = tsf
                    nv = odictn[tsfi][2:]
                    ilistd[i] = nv
                    if n == xylen:
                        restart = False
                    else:
                        restart = True
                        ilistdo = []
                        ilistdo = print_list(ilistd, ilistdo)
                        ilistd = ilistdo
                        break
            if not restart:
                odictf[key] = ilistd
                break
    return (odictf)
#
############################################################################
# subroutine name: rep_dm
#
# purpose:         Expand all D sequences in string, replacing nested
#                  D sequences with full listings of B Descriptors
#
# input:           oseqs   -   String of Tabled and Table B descriptors.
#
#                  odref   -   dictionary of expanded sequences taken
#                              from the MetDB Table D
#
# output:          oseqd   -   list containing expanded D sequence.
#
#
############################################################################


def rep_dm(oseqs, odref):
    oseqd = []
    tsd = ' '.join(oseqs.split())
    tsfd = tsd.split()
    D_test = "True"
    while D_test:
        restart = False
        n = 0
        xylen = len(tsfd)
        for ix in range(0, xylen):
            n = ix
            ts = tsfd[ix][0]
            tsf = tsfd[ix]
            if ts == "3":
                tsfi = tsf
                nvx = odref[tsfi][2:]
                tsfd[ix] = nvx
                if n == xylen:
                    restart = False
                else:
                    restart = True
                    break
        if not restart:
            break
    ddout = {}
    ddout[oseqs] = tsfd[0]
    return (ddout)
#
#############################################################################
# subroutine name: print_list
#
# purpose:         Subroutine called by "straight" to identify and replace
#                  nested lists. The instance method will return true
#                  if the object matches the type specified , in this
#                  case "List".
#
#                  therefore a nested list such as -
#
#                   ["23","24","25",["26","27","28"],"29","30"]
#
#                            will be transformed to
#
#                   ["23","24","25","26","27","28","29","30"]
#
# input:           the_list    -  list which may (or may not) contain
#                                 nested lists
#
# returns:         dn          -  output lists
#
#
#############################################################################


def print_list(the_list, dn):
    for each in the_list:
        if isinstance(each, list):
            print_list(each, dn)
        else:
            dn.append(each)
    return(dn)
#
##############################################################################
# subroutine name: straight
#
# purpose:         Will search for nested lists (within the main list) and
#                  merge the nested data at the right point. The end result
#                  will be one continous list. Note this subroutine includes
#                  a call to the subroutine print_list.
#
# input:           idict    -  expanded D sequence returned by rep_d.
#
# returns:         tempd      - dictionary of lists corresponding to an
#                               expanded Table D Sequence
#                               cross referenced against Tables B & C.
#
#
##############################################################################


def straight(idict):
    for key in list(idict.keys()):
        tdfn = []
        tdfo = []
        tdfn = idict[key]
        tdf = print_list(tdfn, tdfo)
        idict[key] = tdf
    return (idict)
#
#############################################################################
# subroutine name: read_tablec
#
# purpose:         read in the contents of BUFR Table C to a dictionary
#                  of lists. The dictionary will be of the form
#                  tabc{key :[ 'Descriptor' ,'Details',' ',' ']}
#
# input:           cin       -  text version of Table C
#
# returns:         tabc      - dictionary of lists corresponding to Table C
#
#
##############################################################################


def read_tablec(cin):
    tabc = {}
    clist = []
    tdc = open(cin, 'r')
    for line in tdc:
        ctemp = []
        at = line.replace(";", " ")
        a = at.replace("\",,,\"", ";;;")
        b = a.replace("\",,\"", ";;")
        c = b.replace("\",\"", ";")
        d = c.replace("\",", ";")
        e = d.replace(",\"", ";")
        cline = e
        clines = cline.split(";")
        key = clines[1]
        cdesc = clines[2]
        cdetails = clines[3]
        blank1 = "na"
        ctemp.append(cdesc)
        ctemp.append(cdetails)
        ctemp.append(blank1)
        ctemp.append(blank1)
        tabc[key] = ctemp
    return (tabc)
#
###############################################################################
# subroutine name: cross_ref
#
# purpose:         cross_reference an expanded D sequence against
#                  BUFR Table C
#
# input:           bdictin    -  dictionary of lists containing Table B
#                  ddictin    -  dictionary of lists containing Table D
#                  cdictin    -  dictionary of lists containing Table C
#
# returns:         tempd      - dictionary of lists corresponding to an
#                               expanded Table D Sequence
#                               cross referenced against Tables B & C..
#
#
###############################################################################


def cross_ref(bdictin, ddictin, cdictin):
    tempd = {}
    for key in list(ddictin.keys()):
        tlist = []
        tlist = ddictin[key]
        tlen = len(tlist)
        tent = []
        for y in range(0, tlen):
            bdet = []
            bkey = tlist[y]
            bkeyt = bkey
            if bkey[0] == "2":
                bkeyf = int(bkey[:3])
                bent = []
                if bkeyf >= 200 and bkeyf < 222:
                    bnval = bkey[3:]
                    bnkey = bkey[:3]+"YYY"
                    bdet = cdictin[bnkey]
                    bdets = bdet[1]
                    bdetn = bdets.replace("YYY", bnval)
                    bent.append(bkey)
                    bent.append(bdetn)
                    bent.append(bdet[2])
                    bent.append(bdet[3])
                    tent.append(bent)
                else:
                    bdet = cdictin[bkeyt]
                    tent.append(bkey)
                    tent.append(bdet)
            elif bkey[0] == "1":
                bdet = []
                descno = bkey[1:3]
                repno = bkey[3:6]
                repnon = int(repno)
                if repnon == 0:
                    title = "Delayed Replication of next " \
                             + descno + " descriptors"
                else:
                    title = "Relication of next " \
                             + descno + " descriptors " + repno + " times"
                bent = []
                bent.append(bkey)
                bent.append(title)
                tent.append(bent)
            else:
                bdet = bdictin[bkeyt]
                bdesc = bdet[0][1]
                bunit = bdet[0][2]
                bscale = bdet[0][3]
                bref = bdet[0][4]
                bent = []
                bent.append(bkey)
                bent.append(bdesc)
                bent.append(bunit)
                bent.append(bscale)
                bent.append(bref)
                tent.append(bent)
        tempd[key] = tent
    return (tempd)
#
############################################################################
# subroutine name: out_seq
#
# purpose:         Output Expanded Sequence
#
# input:           fdict      -  Expanded D Sequence , cross referenced
#                                against Table C. Note this is output
#                                from cross_ref.
#
#                  seqID      -  D Sequence which has been expanded.
#
# returns:          Nil.
#
# Output:          D_Sequence_Sequence_ID_output_YYDDMM_HHMM.txt
#
#
############################################################################


def out_seq(fdict, seqID, build_dir):
    i = datetime.now()
    xt = i.strftime('%Y-%m-%d_%H:%M:%S')
    xp = i.strftime('%d/%m/%Y %H:%M:%S')
    pwd = os.getcwd()
    fcs_out = build_dir+"/output/D_Sequence_"+seqID+"_output_"+xt+".txt"
    fcs = open(fcs_out, 'w')
    seqID2 = seqID
    for key in list(fdict.keys()):
        ab = type(key)
    seq_out = []
    seq_out = fdict[seqID2]
    slen = len(seq_out)
    mkey_out = '{message:{fill}{align}{width}}'.format(
                 message=seqID, fill=' ', align='<', width=15)
    fcs.write(('%s\n') % (mkey_out))
    for x in range(0, slen):
        keyd = seq_out[x][0]
        dID = seq_out[x][1]
        key_out = '{message:{fill}{align}{width}}'.format(
                    message=keyd, fill=' ', align='<', width=15)
        dID_out = '{message:{fill}{align}{width}}'.format(
                    message=dID, fill=' ', align='<', width=40)
        fcs.write(('%s') % (key_out))
        fcs.write(('%s\n') % (dID_out))
    fcs.close()
    print("================================================================\n")
    print("\n")
    print(" D Sequence ", seqID, " Expanded output to - ", fcs_out, "\n")
    print("\n")
    print("================================================================\n")
    return()
#
############################################################################
#
#  read_all_part:
#
#  input    : filein: element index
#             dxin  : dictionary output from the reading of tabled
#
#  calls   : analyse_part_one
#            analyse_part_two
#            analyse_part_three
#
#  output  : adfx - Dictionary constructed from the merging of 
#            adf1 - dictionary of part1 of the element listing_test
#            adf2 - nested lists of part2 of the element listing.
#            The structure of each entry will be :
#            key = index_no - Sequential No 
#            adfx[key] = [ [ expanded sequence],[Sgn, Sgl],[Sgn, Sgl]]
#            where Sgn = Segment No
#                  Sgl = Length Segment        
#
#  Purpose:  Read in and process the new format elements index in
#            three parts. Seperate the parts and read them into
#            dictionarys / lists.
#
#############################################################################


def read_all_part(filein, dxin): 
    tqt = []
    tqx = []
    pt1 = []
    pt2 = []
    pt3 = []
    adfx = {}
    adf1 = {}
    adf3 = {}
    stx1 = 'BUFR SEQUENCES'
    stx2 = 'INDEX  1'
    stx3 = 'ELEMENT NAMES AND LOCATIONS'
    stx4 = ' (blank line indicates end of list of element names)'
    with open(filein, 'r') as fqz:
        nx = 0
        for linexq in fqz:
            sdx = linexq.lower()
            tqx.append(linexq)
            if linexq.find(stx1) > -1:
                pt1.append(nx+2)
            elif linexq.find(stx2) > -1:
                pt1.append(nx)
                pt2.append(nx)
            elif linexq.find(stx3) > -1:
                pt2.append(nx)
                pt3.append(nx)
            elif sdx.find(stx4) > -1:
                pt3.append(nx)
            nx += 1
        tqt.append(pt1)
        tqt.append(pt2)
        tqt.append(pt3)
        fqz.close()
    for xz in range(0, len(tqx)):
        t1 = tqt[0][0]
        t2 = tqt[0][1]
        t3 = tqt[1][0]
        t4 = tqt[1][1]
        t5 = tqt[2][0]
        t6 = tqt[2][1]
        part1 = tqx[t1:t2]
        part2 = tqx[t3:t4]
        part3 = tqx[t5:t6]
#
    adf1 = analyse_part_one(part1, filein, dxin)
#
    adf2 = analyse_part_two(part2, filein, dxin)
#
    adf3 = analyse_part_three(part3, filein, dxin)
#
    for zval in list(adf1.keys()):
        zvt = adf1[zval]
        zvalx = zval.split('-')
        zx2 = int(zvalx[0])-1
        if len(adf2) > 0:
            xval = adf2[zx2]
            zvt.append(xval)
            adfx[zval] = zvt
#
    return(adfx, adf1, adf3)
#
##############################################################################
#
#  analyse_part_one
#
#  Input:
#
#       partx1:   Part1 of the Elements index as read by the :
#                 DEF : 'read_all_part'. This will be in the form of a
#                 list with each line as a seperate element.
#
#       fzin:     Name and location of the elements_index_file from which
#                 'Part1' has been extracted
#
#       dzref:    Dictionary with D sequences expanded to their
#                    constituent B descriptors
#
#  Output:
#
#       xdictf3:  Dictionary of sequences from within the elements_index_file
#                 expanded to their constituent B descriptors. Each element
#                 of the dictionary output is a list to which the key will
#                 be of the form 'index-Number' - 'sequential Number'
#                  
###############################################################################


def analyse_part_one(partx1, fzin, dzref):
    idict = {}
    nx = 1
    tlist = []
    cnlist = []
#
    for xv in range(0, len(partx1)):
        tval1 = ' '.join(partx1[xv][:12].split())
        tval2 = ' '.join(partx1[xv].split())
        if len(tval1) > 0:
            cnlist.append(xv)
        elif len(tval2) == 0:
            cnlist.append(xv)

    xpout = []
    xpf = []
    for xp in range(0, len(cnlist)-1):
        xp1 = cnlist[xp]
        xp2 = cnlist[xp+1]
        xpout.append(xp1)
        xpout.append(xp2)
        xpf.append(xpout)
        xpout = []
#
    nx = 0
    xdict = {}
    for xz in range(0, len(xpf)):
        nx += 1
        cp1 = xpf[xz][0]
        cp2 = xpf[xz][1]
        nxp = ' '.join(partx1[cp1][:12].split())
        if len(nxp) > 0:
            nent = nxp.split()
            ntx = str(nent[0]) + '-' + str(nx)
        subx = partx1[cp1:cp2]
        subx2 = subx[0][12:]
        subx[0] = subx2
        xdict[ntx] = subx
        xdictf = xdict
#
    xdictf2 = {}
    for xy in list(xdictf.keys()):
        subz = xdictf[xy]
#
        dlist = []
        for xb in range(0, len(subz)):
            nemt = subz[xb]
            nemt2 = nemt.strip('\n')
            nx1 = nemt2.find('-')
            nx2 = nemt2.find('(')
            if nx1 > -1:
                nemt3 = nemt2[:nx1]
                nemt3s = nemt3 + ' '
                dlist.append(nemt3s)
            elif nx2 > -1:
                nemt4 = nemt2[:nx2]
                nemt4s = nemt4 + ' '
                dlist.append(nemt4s)
            else:
                nemt2s = nemt2 + ' '
                dlist.append(nemt2s)
        dlist2 = ''.join(dlist)
        dlist3 = dlist2.split()
        xdictf2[xy] = dlist3
    xdictf3 = rep_ds(xdictf2, dzref, fzin)
#
    return(xdictf3)

##############################################################################
#
#  analyse_part_two
#
#  Input:
#
#       partx2:   Part2 of the Elements index as read by the :
#                 DEF : 'read_all_part'. This will be in the form of a
#                 list with each line as a seperate element.
#
#       fzin:     Name and location of the elements_index_file from which
#                 'Part2' has been extracted
#
#       dzref:    Dictionary with D sequences expanded to their
#                 constituent B descriptors
#
#  Output:
#
#       dref:     List of nested lists each containing the INDEX listing 
#                 of the elements index. Each entry will be of the form 
#                 [[sgn,nent],[sgn,nent],....]
#                 where sgn == segment no and nenet = number of descriptors.       
#                  
###############################################################################


def analyse_part_two(partx2, fzin, dzref):
    tlt = []
    idict2 = {}
    Inx = 'INDEX'
    mplist2 = []
    for xz in range(0, len(partx2)):
        test1 = ' '.join(partx2[xz][0:20].split())
        if test1.find(Inx) > -1:
            tlt.append(xz)
    tlt.append(len(partx2))
    dxz = []
#
    for xz2 in range(0, len(tlt)):
        if xz2 < len(tlt)-1:
            cdx1 = tlt[xz2]
            cdx2 = tlt[xz2+1]
            cdx2a = cdx2
            cdx2b = cdx2a - 3
            dfx = []
            dfx.append(cdx1)
            dfx.append(cdx2b)
            dxz.append(dfx)
            dfx = []
    dref = []
#
    for xz2 in range(0, len(dxz)):
        cx1 = dxz[xz2][0]+4
        cx2 = dxz[xz2][1]
        tlistz = []
        tlist = partx2[cx1:cx2]
        for xc in range(0, len(tlist)):
            tcxs = tlist[xc].strip('\n')
            tcx = ' '.join(tcxs.split())
            csz = tlist[xc][0:12]
            tcx = ' '.join(csz.split())
            tcx.lstrip()
            tcx2 = tcx.split()
            tlistz.append(tcx2)
        dref.append(tlistz)  
#
    return(dref)
#
##############################################################################
#
#  analyse_part_three
#
#  Input:
#
#       partx3:   Part3 of the Elements index as read by the :
#                 DEF : 'read_all_part'. This will be in the form of a
#                 list with each line as a seperate element.
#
#         fzin:   Name and location of the elements_index_file from which
#                 'Part2' has been extracted
#
#        dzref:   Dictionary with D sequences expanded to their
#                 constituent B descriptors
#
#       Output:
#
#         dref:   Dictionary  of nested lists each containing the listing 
#                 of the elements index. Each entry will be of the form 
#                 keyx : [[sgn,S1-ent,P1-ent],[sgn,S2-ent,P2-ent],....]
#                 where Keyx   ==  element name 
#                       sgn    ==  segment no 
#                       S1-ent ==  S1 entry_no
#                       P2-ent == P2 entry                                      
#                  
###############################################################################


def analyse_part_three(partx3, fzin, dzref):
    cdict = {}
    zname = '      ELEMENT NAME      T ID'
    znamex = ' '.join(zname.split())
    yname = '                            '
    swx = -1
    swx2 = -1
    for zx in range(0, len(partx3)):
        linez = partx3[zx]
        linezx = ' '.join(linez.split())
        zx1 = linezx.find(znamex)
        if zx1 > -1:
            (cd_list, rfv) = set_cd(linez)
            zfx = zx + 2
            break
#
    for zd in range(zfx, len(partx3)):
        linezd = partx3[zd]
        xline = linezd[rfv:]
        xcatg = linezd[:24].strip()
        xn = 0
        fent = []
        for xc in range(0, len(cd_list), 2):
            xn += 1
            nx1 = cd_list[xc][0]
            nx2 = cd_list[xc][1]
            xdx = xline[nx1:nx2]
            xcx = xc + 1
            nx3 = cd_list[xcx][0]
            nx4 = cd_list[xcx][1]
            xdx2 = xline[nx3:nx4]
            txz = []
            xdxs = xdx.strip()
#            
            if len(xdxs) > 0:
                if int(xdx) > 0:
                    txz.append(xn)
                    txz.append(xdx)
                    txz.append(xdx2)
                    fent.append(txz)
                    cdict[xcatg] = fent
        fent = []             

    return(cdict)
#
#
#############################################################################
#
# final_proc
#
#  input   : adfx Dictionary of nested lists of the structure :
#            key = index_no - Sequential No 
#            adfx[key] = [ [ expanded sequence],[Sgn, Sgl],[Sgn, Sgl]]
#            where Sgn = Segment No
#                  Sgl = Length Segment 
#
#   dref   : List of nested lists each containing the INDEX listing 
#            of the elements index. Each entry will be of the form 
#            [[sgn,nent],[sgn,nent],....]
#            where sgn == segment no and nenet = number of descriptors. 
#
#            An example being - 
#    
#	     key :    HSB_MAIN_LAND_TYPE_FRCN   [[1, ' 9', ' 3']]
#                     MNTH                      [[1, ' 1', '26']]
#                     HSB_INST_TMPR             [[1, ' 1', '23']]
#
#   filex  : Name & location of the element index file
#            example  /home/developers/richard.weedon/BUFRCREX_32_0_0/
#
#   d3exp  : fully expanded tabled sequences
#            (including local sequences)
#
#   output : dictionary of lists structured as below -
#
#            where key = FXY 
#                  dfinal[FXY] = [ corresponding element retrieval names] 
#
##############################################################################


def final_proc(dd2, dref, filex, d3exp):
    dd2rf = {}
    dfinal = {}
    dx = 0
    for kx2 in list(dd2.keys()):
        dlref = dd2[kx2][-1:][0]
        dsre = dd2[kx2][:-1]
        inc = 0
        dlx = 0
        dxn = []
        dxn2 = []
        for xz in range(0, len(dlref)):
            if len(dlref[xz]) > 0:
                if xz == 0:
                    dlx = 0
                    dxp = int(dlref[xz][1])
                    dxn.append(dlx)
                    dxn.append(dxp)
                    dxn2.append(dxn)
                else:
                    dxn.append(dxp)
                    dxp2 = int(dxp) + int(dlref[xz][1])
                    dxn.append(dxp2)
                    dxp = dxp2
                    dxn2.append(dxn)
                dxn = []
        dsre.append(dxn2)
        dd2rf[kx2] = dsre
#
    ddx3 = {}
    for kz2 in list(dd2rf.keys()):
        dqz = dd2rf[kz2][-1:][0]
        if len(dqz) > 0:
            dsre = dd2rf[kz2][:-1]
            refx = len(dd2rf)
            diffx = []
            rfst = []
            dx = '0'
            for xy in range(0, len(dsre)):
                dfqs = dsre[xy].strip()
                dfq = dfqs[0].find('3')
                dfx = dfqs[0].find('2')
                dfy = dfqs[0].find('1')
                dfz = dfqs[0:3].find('031')
                if dfq > -1:
                    if dsre[xy] in list(d3exp.keys()):
                        n3ent = d3exp[dsre[xy]]
                        diffx.append(n3ent)
                        diffot = []
                        dout = print_list(diffx, diffot)
                        diffx = diffot
                    else:
                        print("d sequence not found ", dsre[xy], "\n")
                        dx = '9999'
                        break
                if dfx == -1:
                    if dfy == -1:
                        if dfz == -1:
                            diffx.append(dsre[xy])
            if dx != '9999':
                diffx.append(dqz)
                ddx3[kz2] = diffx
            else:
                return(dfinal, dx)
#
    ddx4 = {}
    for keyx3 in list(ddx3.keys()):
        qvlist = ddx3[keyx3][:-1]
        nvlist = ddx3[keyx3][-1:]
        fdlist = []
        for xv in range(0, len(nvlist[0])):
            nvq1 = nvlist[0][xv][0]
            nvq2 = nvlist[0][xv][1]
            if nvq1 < len(qvlist):
                if nvq2 < len(qvlist):
                    tlist = qvlist[nvq1:nvq2]
                    fdlist.append(tlist)
                else:
                    tlist = qvlist[nvq1:]
                    fdlist.append(tlist)
            ddx4[keyx3] = fdlist
#
    dfinal = {}
    for kx2 in list(dref.keys()):
        xval = dref[kx2]
        for xv in range(0, len(xval)):
            inval = xval[xv][0]
            sgval = int(xval[xv][1])-1
            psnval = int(xval[xv][2])-1
            inxvl = str(inval) + '-'
            for kz in list(ddx4.keys()):
                xtn = kz[:2]
                if xtn == inxvl:
                    xz = int(inval) - 1
                    if xz < len(ddx4[kz]):
                        fval = ddx4[kz]
                        if sgval < len(fval):
                            if psnval < len(fval[sgval]):
                                sdx = fval[sgval][psnval]
                                if sdx in list(dfinal.keys()):
                                    dtemp = dfinal[sdx]
                                    if kx2 not in dtemp[1:]:
                                        dtemp.append(kx2)
                                else:
                                    dtemp = []
                                    dtemp.append(kx2)
                                dfinal[sdx] = dtemp
#
    return(dfinal, dx)
#
#
#############################################################################


def mergeDict(rdict1, mdict2):
    for keyr in list(rdict1.keys()):
        if keyr in list(mdict2.keys()):
            rlist1 = rdict1[keyr]
            mlist2 = mdict2[keyr]
            flist2 = rlist1 + mlist2
        else:
            flist2 = rdict1[keyr]
        flist2x = list(set(flist2))
        mdict2[keyr] = flist2x
#
    for keyx in list(mdict2.keys()):
        if keyx not in list(mdict2.keys()):
            mlist2 = mdict2[keyx]
            mdict2[keyx] = mlist2
#
    return(mdict2)
#
##############################################################################
#
#  subroutine_name: assemble_ret_names
#
#  input    : refxz - list of element index files
#
#  output   : dxdz - dictionary { B descriptor (FXY): [list of element
#                    name(s) matched against the descriptor)
#
#  calls    read_all_part
#	    final_proc
#           mergeDict
#
#
###############################################################################


def assemble_ret_names(tdrx, refxz):
    sdfx = {}
    refzc = []
    xdict = {}
    dtlist = []
    chz = -1
    with open(refxz, 'r') as vxz:
        for linexz in vxz:
           refzc.append(linexz)
        vxz.close() 
    for xz in range(0, len(refzc)):
        filein = refzc[xz].strip()
        (sxdict, xpd3, refdict) = read_all_part(filein, tdrx)
        abvname = filein.split('/')
        abfname = abvname[-1:][0][:-4]
        (refd, xzv) = final_proc(sxdict, refdict, filein, tdrx)
        if xzv != '9999':
            mxdict2 = mergeDict(refd, xdict)
            xdict = mxdict2 
    for keyzx in list(xdict.keys()):
        fxname = xdict[keyzx]
        fxname2 = list(set(fxname))
        xdict[keyzx] = fxname2
    return(xdict)
#
##############################################################################
# subroutine name: index_cr
#
# purpose:         Output skeleton of the elements index for a specific
#                  D sequence
#
# input:           fdict      -  Expanded D Sequence , cross referenced
#                                against Table C. Note this is output from
#                                cross_ref
#
#                  seqID      -  D Sequence which has been expanded.
#
#                  build_dir  -  output directory
#
#                  refid      -  dictionary containing TABLE B
#                                 [FXY] = ['RETRIEVAL NAME1',
#                                         'RETRIEVAL NAME2'.....]
#
# output:          Element_Index_SeqID_output_YYMMDD_HHMM.txt
#
#
##############################################################################


def index_cr(fdict, seqID, build_dir, refid):
    i = datetime.now()
    xt = i.strftime('%Y-%m-%d_%H:%M:%S')
    xp = i.strftime('%d/%m/%Y %H:%M:%S')
    SeqID = str(seqID)
    pwd = os.getcwd()
    out_file2 = build_dir+"/output/Element_Index_"+SeqID+"_output_"+xt+".txt"
    seq_in = []
    seqID2 = seqID
    seq_in = fdict[seqID2]
    seq_out = []
    si_len = len(seq_in)
    with open(out_file2, 'w') as q:
        seg = []
        segf = []
        pos = 1
        repn = 1
        segn = 1
        for k in range(0, si_len):
            desc = seq_in[k]
            keyi = desc[0]
            if keyi[0] == "1":
                pos = 1
                segn += 1
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                desc.append(repn)
                repn += 1
                repc = int(keyi[3:])
                if repc == 0:
                    k += 1
                    desc2 = []
                    desc2 = seq_in[k]
                    if desc2[0] == "031001" or desc2[0] == "031002":
                        desc.append("-1")
                    else:
                        desc.append("-2")
                    desc2.append(" ")
                    desc2.append(" ")
                    desc2.append(" ")
                    desc2.append(" ")
                    desc.append(" ")
                    seq_out.append(desc)
                    seq_out.append(desc2)
                else:
                    desc.append(repc)
                    seq_out.append(desc)
            elif keyi[0] == "2":
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                desc.append(" ")
                seq_out.append(desc)
            elif keyi[0] == "0" and keyi[:3] != "031":
                pos2 = str(pos)
                desc.append(pos2)
                desc.append(segn)
                desc.append(" ")
                desc.append(" ")
                seq_out.append(desc)
                pos += 1
        dcpt = '{message:{fill}{align}{width}}'.format(
                 message="Descriptor", fill=' ', align='<', width=14)
        dta = '{message:{fill}{align}{width}}'.format(
                message="Data", fill=' ', align='<', width=84)
        sgmt = '{message:{fill}{align}{width}}'.format(
                message="Segment", fill=' ', align='<', width=10)
        pstn = '{message:{fill}{align}{width}}'.format(
                 message="Position", fill=' ', align='<', width=11)
        rptn = '{message:{fill}{align}{width}}'.format(
                 message="Replication", fill=' ', align='<', width=14)
        count = ' {message:{fill}{align}{width}}'.format(
                   message="Count", fill=' ', align='<', width=8)
        linespace = '{message:{fill}{align}{width}}'.format(
                      message="=", fill='=', align='>', width=140)
        q.write(('%s') % (dcpt))
        q.write(('%s') % (dta))
        q.write(('%s') % (sgmt))
        q.write(('%s') % (pstn))
        q.write(('%s') % (rptn))
        q.write(('%s\n') % (count))
        q.write(('%s\n') % (linespace))
#        for t in range(0, si_len):
        for t in range(0, len(seq_out)):
            keyd = seq_out[t][0]
            dscrip = seq_out[t][1]
            if len(dscrip) >= 45:
                dscrip = dscrip[0:44]
            segmnt = seq_out[t][6]
            posnt = seq_out[t][5]
            repn = seq_out[t][7]
            count = seq_out[t][8]
            key_out = '{message:{fill}{align}{width}}'.format(
                         message=keyd, fill=' ', align='<', width=14)
            desc_out = '{message:{fill}{align}{width}}'.format(
                         message=dscrip, fill=' ', align='<', width=86)
            sgmt_out = '{message:{fill}{align}{width}}'.format(
                         message=segmnt, fill=' ', align='<', width=10)
            pstn_out = '{message:{fill}{align}{width}}'.format(
                         message=posnt, fill=' ', align='<', width=11)
            rptn_out = '{message:{fill}{align}{width}}'.format(
                         message=repn, fill=' ', align='<', width=14)
            count_out = '{message:{fill}{align}{width}}'.format(
                          message=count, fill=' ', align='<', width=8)
            line_out = '{message:{fill}{align}{width}}'.format(
                          message=linespace, fill=' ', align='<', width=14)
            if keyd[0] == "1":
                q.write(('%s') % (key_out))
                q.write(('%s') % (desc_out))
                q.write(('%s') % (sgmt_out))
                q.write(('%s') % (pstn_out))
                q.write(('%s') % (rptn_out))
                q.write(('%s\n') % (count_out))
                q.write(('%s\n') % (linespace))
                if keyd[3:] == "000":
                    t += 1
                    keyd = seq_out[t][0]
                    dscrip = seq_out[t][1]
                    segmnt = seq_out[t][6]
                    posnt = seq_out[t][5]
                    repn = seq_out[t][7]
                    count = seq_out[t][8]
                    key_out = '{message:{fill}{align}{width}}'.format(
                                message=keyd, fill=' ', align='<', width=14)
                    desc_out = '{message:{fill}{align}{width}}'.format(
                                 message=dscrip, fill=' ', align='<', width=86)
                    sgmt_out = '{message:{fill}{align}{width}}'.format(
                                 message=segmnt, fill=' ', align='<', width=10)
                    pstn_out = '{message:{fill}{align}{width}}'.format(
                                 message=posnt, fill=' ', align='<', width=11)
                    rptn_out = '{message:{fill}{align}{width}}'.format(
                                 message=repn, fill=' ', align='<', width=14)
                    count_out = '{message:{fill}{align}{width}}'.format(
                                  message=count, fill=' ', align='<', width=8)
                    q.write(('%s') % (key_out))
                    q.write(('%s') % (desc_out))
                    q.write(('%s') % (sgmt_out))
                    q.write(('%s') % (pstn_out))
                    q.write(('%s') % (rptn_out))
                    q.write(('%s\n') % (count_out))
                    q.write(('%s\n') % (linespace))
                else:
                    line_out = '{message:{fill}{align}{width}}'.format(
                                 message=linespace, fill=' ', align='<',
                                 width=14)
                    q.write(('%s\n') % (linespace))
            elif keyd[:3] != "031" and keyd[0] == "0":
                q.write(('%s') % (key_out))
                q.write(('%s') % (desc_out))
                q.write(('%s') % (sgmt_out))
                q.write(('%s') % (pstn_out))
                q.write(('%s') % (rptn_out))
                q.write(('%s\n') % (count_out))
#
        dcpt = '{message:{fill}{align}{width}}'.format(
                 message="FXY Value", fill=' ', align='<', width=20)
        sgdn = '{message:{fill}{align}{width}}'.format(
                 message="Element Retrieval Names", fill=' ',
                 align='<', width=20)
        fspc = '{message:{fill}{align}{width}}'.format(
                 message="=", fill='=', align='<', width=80)
        q.write(('%s\n') % (" "))
        q.write(('%s\n') % (fspc))
        q.write(('%s') % (dcpt))
        q.write(('%s\n') % (sgdn))
        q.write(('%s\n') % (fspc))
#
        for xz in range(0, len(seq_in)):            
            tval = seq_in[xz][0]
            if tval in list(refid.keys()):
                trec = refid[tval]
                twx = '{message:{fill}{align}{width}}'.format(
                        message=tval, fill=' ', align='<', width=20)
                q.write(('%s') % (twx))
                for xpx in range(0, len(trec)):
                    xpn = xpx % 3
                    tdf = len(trec[xpx]) + 1
                    tzf = '{message:{fill}{align}{width}}'.format(
                           message=trec[xpx], fill=' ', align='<', width=tdf)
                    tzq = '{message:{fill}{align}{width}}'.format(
                           message=' ', fill=' ', align='<', width=20)
                    if xpx < len(trec) - 1:
                        if xpn == 2:
                            q.write(('%s\n') % (tzf))
                            q.write(('%s') % (tzq))
                        elif xpn < 2:
                            q.write(('%s') % (tzf))
                    else:
                        q.write(('%s\n') % (tzf))
                q.write(('%s\n') % (' '))
#

        idxl = len(seqID)+10
        idx = '{message:{fill}{align}{width}}'.format(
                message=seqID, fill=' ', align='<', width=20) 
        fspc2 = '{message:{fill}{align}{width}}'.format(
                 message="=", fill='=', align='<', width=90)
        dcpt2 = '{message:{fill}{align}{width}}'.format(
                 message="FXY", fill=' ', align='<', width=20)
        dcpt3 = '{message:{fill}{align}{width}}'.format(
                 message="Expanded Encoding Sequence", fill=' ', 
                 align='<', width=30)
        tzq = '{message:{fill}{align}{width}}'.format(
                message=' ', fill=' ', align='<', width=20)
#
        q.write(('%s\n') % (fspc2))
        q.write(('%s') % (dcpt2))
        q.write(('%s\n') % (dcpt3))
        q.write(('%s\n') % (tzq))
        q.write(('%s\n') % (fspc2))
        for xpz in range(0, len(seq_in)):  
            xpq = xpz % 8  
            idxp = '{message:{fill}{align}{width}}'.format(
                message=seq_in[xpz][0], fill=' ', align='<', width=8)
            if xpz < len(fdict) - 1:
                if xpz == 0 and xpq == 0:
                    q.write(('%s') % (idx)) 
                    q.write(('%s') % (idxp))  
                elif xpz > 0 and xpq == 7:
                    q.write(('%s\n') % (idxp))
                    q.write(('%s') % (tzq))
                elif xpz > 0 and xpq < 7:
                    q.write(('%s') % (idxp))       
            else:
                q.write(('%s\n') % (idxp))                    
        q.write(('%s\n') % (' '))
    q.close()
    print("===============================================================\n")
    print("\n")
    print(" Element Index output - ", out_file2, "\n")
    print("\n")
    print("===============================================================\n")
    return()
#
############################################################################
#
# subroutine name: rep_ds
#
# purpose:         Expand all D sequences, replacing nested D sequences
#                  with full listings of B Descriptors
#
# input:           odictn     -
#
# output:          odictf  -
#
#
############################################################################


def rep_ds(odictn, dxref, fzx):
    odictf = {}
    for key in list(odictn.keys()):
        ilistd = []
        ilistd = odictn[key]
        D_test = "True"
        while D_test:
            restart = False
            n = 0
            xylen = len(ilistd)
            for i in range(0, xylen):
                n = i
                ts = ilistd[i][0]
                tsf = ilistd[i]
                if ts == "3":
                    tsfi = tsf
                    if tsfi in list(dxref.keys()):
                        nv = dxref[tsfi]
                        ilistd[i] = nv
                    else:
                        break
                    if n == xylen:
                        restart = False
                    else:
                        restart = True
                        ilistdo = []
                        ilistdo = print_list(ilistd, ilistdo)
                        ilistd = ilistdo
                        break
            if not restart:
                odictf[key] = ilistd
                break
    return (odictf)
############################################################################
#
#  set_cd
#
#  input : single of test extracted from the elements index as below -
#           ' ELEMENT NAME       T ID  S1  P1  S2  P2  S3  P3  S4 '
#
#  output: nlx   - list of integers giving the coordinates of the columns
#                  determined by S1 P1 ... etc (as above)
#          refv  - Integer , offset for the start of the input string at
#                  which the coordinates apply
#
###########################################################################


def set_cd(linezx):
    nxz = linezx.replace(' ', 'x')
    nx = nxz.find('TxIDxx')
    tline = nxz[(nx+6):]
    refv = nx+6
    sdx = [n for n in range(len(tline)) if tline.find('xx', n) == n]
    adx = sdx[-1:][0]
    apx = adx + 2
    apxf = adx + 4
    nlx = []
    for xd in range(0, len(sdx)):
        nlx2 = []
        ncr1 = sdx[xd]-2
        ncr2 = sdx[xd]
        nlx2.append(ncr1)
        nlx2.append(ncr2)
        nlx.append(nlx2)
    nlx3 = []
    nlx3.append(apx)
    nlx3.append(apxf)
    nlx.append(nlx3)
    return(nlx, refv)
#
##############################################################################
# subroutine name: menu
#
# purpose:         Display options and gather input from the user.
#
# input:           Option no - as below
#
#
##############################################################################


def menu(opti):
    inp = opti[0]              # option indicator
    hdiri = opti[1]            # root of the bufr directory
    tablegBx = opti[2]         # WMO version of BUFR Table B (txt)
    tablegDx = opti[3]         # WMO version of BUFR Table D (txt)
    tablegCx = opti[4]         # WMO version of BUFR Table C (txt)
    tablegcct = opti[5]        # WMO version of BUFR code and flag tables(txt)
    tablegcc1 = opti[6]        # WMO version of Common Code tables No (1)
    tablegcc2 = opti[7]        # WMO version of Common Code tables No (2)
    tablegcc5 = opti[8]        # WMO version of Common Code tables No (5)
    tablegc7 = opti[9]         # WMO version of Common Code tables No (7)
    tablegc8 = opti[10]        # WMO version of Common Code tables No (8)
    tableoBt = opti[11]        # MetDB version of BUFR Table B (txt)
    tableoDt = opti[12]        # MetDB version of BUFR Table D (txt)
    tableglct = opti[13]       # MetDB version of the codefig tables (txt)
    seq = opti[14]             # Table D ID
    refxv = opti[15]           # 
#
    if inp == "3":             # Update operational BUFR table B with new entry
        Tno = "B"
        table_check(inp, hdiri, tablegBx, tableoBt, tablegCx)
        (mastergb, wmo_version) = getDatb(tablegBx)
        (masterlb, rev_now) = read_in_tableb(tableoBt)
        (f_dictb) = final_dictb(masterlb, mastergb, wmo_version)
        output_b(f_dictb, rev_now, wmo_version, hdiri)
        comp_versions(mastergb, masterlb, Tno, wmo_version, rev_now, hdiri)
#
    elif inp == "4":
        Tno = "D"
        table_check(inp, hdiri, tablegDx, tableoDt, tablegCx)
        (mastergd, wmo_version) = getDatd(tablegDx)
        (masterld, rev_now) = read_in_tableD(tableoDt)
        (f_dictd) = final_dictd(masterld, mastergd)
        output_d(f_dictd, rev_now, wmo_version, hdiri)
        comp_versions(mastergd, masterld, Tno, wmo_version, rev_now, hdiri)
#
    elif inp == "1":
        Tno = "B"
        table_check(inp, hdiri, tablegBx, tableoBt, tablegCx)
        (mastergb, wmo_version) = getDatb(tablegBx)
        (masterlb, rev_now) = read_in_tableb(tableoBt)
        new_entry = add_entry(Tno, rev_now, masterlb)
        (f_dictb) = final_dictb(new_entry, mastergb, wmo_version)
        output_b(f_dictb, rev_now, wmo_version, hdiri)
#
    elif inp == "2":
        Tno = "D"
        table_check(inp, hdiri, tablegDx, tableoDt, tablegCx)
        (mastergd, wmo_version) = getDatd(tablegDx)
        (masterld, revd) = read_in_tableD(tableoDt)
        (new_entry) = add_entry(Tno, revd, masterld)
        (f_dictd) = final_dictd(new_entry, mastergd)
        output_d(f_dictd, revd, wmo_version, hdiri)
#
    elif inp == "6":
        Tno = "B"
        table_check(inp, hdiri, tablegBx, tableoBt, tablegCx)
        (masterg, newversion) = getDatb(tablegBx)
        (masterb, rev_now) = read_in_tableb(tableoBt)
        comp_versions(masterg, masterb, Tno, newversion, rev_now, hdiri)
#
    elif inp == "7":
        Tno = "D"
        table_check(inp, hdiri, tablegDx, tableoDt, tablegCx)
        (nvers, new_version) = getDatd(tablegDx)
        (overs, revd) = read_in_tableD(tableoDt)
        comp_versions(nvers, overs, Tno, new_version, revd, hdiri)
#
    elif inp == "5":
        table_check(inp, hdiri, tablegcct, tableglct, tablegcct)
        (fdict1, rev_no) = read_oldfig(tableglct)
        fdict2 = read_newfig(tablegcct)
        mfdict = mdict(fdict1, fdict2)
        mf_dict = link_common_tables(mfdict, hdiri)
        out_codefig(mf_dict, rev_no, hdiri)
#
    elif inp == "8":
        table_check(inp, hdiri, tablegDx, tablegBx, tablegCx)
        (tableb, revb) = read_in_tableb(tableoBt)
        (tablec) = read_tablec(tablegCx)
        (tabled, revd) = read_in_tableD(tableoDt)
        (tabled_exp) = rep_d(tabled)
        tabled_final = straight(tabled_exp)
        dictf = cross_ref(tableb, tabled_final, tablec)
        out_seq(dictf, seq, hdiri)
#
    elif inp == "9":
        iplt = []
        table_check(inp, hdiri, tablegDx, tablegBx, tablegCx)
        (tableb, revb) = read_in_tableb(tableoBt)
        (tablec) = read_tablec(tablegCx)
        (tabled, revd) = read_in_tableD(tableoDt)
        (tabled_exp) = rep_d(tabled)
        (tabled_exseq) = rep_dm(seq, tabled_exp)  # is this needed?
    #    exname=assemble_ret_names(tabled_exseq,refxv)   
        exname=assemble_ret_names(tabled_exp,refxv)
        dictf = cross_ref(tableb, tabled_exp, tablec)       
        out_seq(dictf,seq,hdiri)                                                
        index_cr(dictf,seq,hdiri,exname)                                   
#        dictf = cross_ref(tableb, tabled_exp, tablec)
#
    elif inp == "10":
        sys.exit()
    else:
        print("\n Not a valid option, Try again")
#
#################################################################
menu(sys.argv[1:])

