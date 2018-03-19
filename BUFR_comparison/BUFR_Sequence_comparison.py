# -*- coding: iso-8859-1 -*-
import sys
import binascii
import itertools
from datetime import datetime
from time import gmtime, strftime
import datetime as dt
from operator import itemgetter
import time
import os
import os.path
import re
import string
import shlex, subprocess
import copy
#############################################################################
#
# Name:             data_feed_comparison.py
#
# purpose:          Testing of the D sequence used to encode MHSR bulletins
#                   to ensure that a mapping exists with the current element
#                   indexes.
#
# subroutines:      read_c11  =>   Read the txt version of common code table
#                   C11 into dict of lists
#                   parameters:
#                   input =>
#                            file_in - Filename of txt version of common
#                                     code table C11
#
#                   output
#                   c11_read - dict of lists.
#
#                   read_c12  =>   Read the txt version of common code
#                                  table C12 into dict of lists
#                   parameters:
#                   input =>
#                            file_in - Filename of txt version of common
#                                      code table C12
#
#                   output
#                   c12_read - dict of lists.
#
#############################################################################
#
# subroutine : 	    ind_read
#
# purpose:          Read the retrieval elements index (new type)
#
# parameters:
#                   input =>
#                   S1    -   full filename of the retrieval index file.
#
#                   output =>
#                   flist2 - list containing D sequences from the index
#                            file sections 1 - 3 of the MHSR file.
#
#############################################################################
#


def ind_read(S1):
    tlist = []
    n = -1
    B1 = 'BUFR SEQUENCES'
    I1 = 'INDEX'
    lenS1 = len(S1)-1
    n = 0
    slist = []
    flist = []
    plist = []
    start = 0
    with open(S1, 'r') as fi:
                                        # Open the element index as Read only
        iname = S1.split("/")[-1:][0]
        for line in fi:
            line = line.replace("\n", "")
            if line.find(B1) > -1:
                                        # The D sequence values in the
                                        # retrieval index are bounded by
                start = 1
                                        # the strings  'BUFR SEQUENCES
                                        # & 'INDEX'
                line = fi.next()
            elif line.find(I1) > -1 or len(line.replace(" ", "")) == 0:
                                        # end of index values
                flist.append(plist)
                break
            if start == 1:
                tlist = line[0:11].replace(" ", "")
                                        # test for new sequence
                slist = (line[12:]. split(' '))
                if len(tlist) > 0:
                                        # start of new index sequence
                    if len(plist) != 0:
                        if n > 0:
                            flist.append(plist)
                            plist = []
                        plist = slist
                    else:
                        plist = slist
                else:
                    plist = plist+slist
            n = n+1
        flist2 = flist[1:]
    fi.close()
    return(flist2)
#
#############################################################################
#
#
# subroutine : 	    sum_bin
#
# purpose:          Will read (as bin) sections 1 - 3 of the MHSR file.
#                   Section 0 is identical in BUFR editions 3 & 4.
#                   The intial part of the code will not differentiate
#                   between these for this section
#
# parameters:
#                   input =>
#	            S2      -  full filename of the MHSR file.
# 	            c11_d   -  filename common code table C11
#                   c12_d   -  filename common code table C12
#                   errord  -  Error dictionary
#
#                   output => sum_dict - dict containing Decoded (bin - ascii)
#                                        data from sections 1 - 3 of the MHSR
#                                        file.
#                             errq     - dictionary containing error messages
#                             uttc     - flag (set to either 1 or -1) to
#                                        indicate the presence of an
#                                        encoding D sequence
#
#
#############################################################################
#
#
def sum_bin(S2, c11_d, c12_d, errord):
#
#   hdef = Hex equivalents of - ASCII
    hdef = [['4945'], ['494e'], ['494f'], ['4950'], ['4953'],\
           ['4954'], ['4955']]
    dint = []
    errm = ['None', 'None', 'None', 'None', 'None', 'None', 'None']
    uttc = -1
#
    tstring = ''
    sum_dict = {}
    with open(S2, 'rb') as fp:
        data = fp.read(1)
        while data != "":
            data = fp.read(1)
            fstring1 = binascii.b2a_hex(data)
            tstring2 = tstring + fstring1
            tstring = tstring2
        fp.close()
#
    tlistm = []
    tlistq = []
#
    for m in re.finditer('42554652', tstring):
        tlistd = []
        tlistd.append(m.start())
        tlistd.append(m.end())
        tlistm.append(tlistd)
#
    for q in re.finditer('37373737', tstring):
        tlistd2 = []
        tlistd2.append(q.start())
        tlistd2.append(q.end())
        tlistq.append(tlistd2)
#
    if len(tlistq) == len(tlistm):      # pair corresponding start and
                                        # finish index pairs
        nflist = []
        for z in range(0, len(tlistm)):
            nlist = []
            if z == 0:
                stp = 0
                ep = tlistq[z][1]
                nlist.append(stp)
                nlist.append(ep)
            else:
                stp = ep + 1
                ep = tlistq[z][1]
                nlist.append(stp)
                nlist.append(ep)
            nflist.append(nlist)
    else:                               # catch error 13  Unable to read BUFR
        pflist = errord['13']
        errmf = errm
        errmf[0] = S2
        pflist.append(errmf)
        errord['13'] = pflist
        uttc = -1
        sum_dict = []
        return(sum_dict, errord, uttc)
#
    rngf = []
    for x in range(0, len(nflist)):
        rng = []
        stb = tstring[nflist[x][0]:nflist[x][1]]
                                        #  extract observations into sub list
        rng.append(stb)
        rngf.append(rng)
        #
    obn = 0
    for yz in range(0, len(rngf)):       # loop through obs in bulletin
        obn += 1
        ob = rngf[yz]
        opf = S2 + '-' + str(obn)
        obp = ob[0].find('42554652')    # find position of BUFR
        obf = len(ob[0])                # find end position of 7777
        odid = ob[0][0:obp]             # start of bulletin => 'BUFR'
        obr = ob[0][obp:obf]            # bulletin 'BUFR => end position
                                        # of 7777
                                        # find ttaaii cccc
                                        # iterate through hdef look for
        ng = 0                          # hex match in string
        for yx in range(0, len(hdef)):     
            if odid.find(hdef[yx][0]) > -1:
                gh = odid.find(hdef[yx][0])
                                        # ttaaii cccc
                ghid = odid[gh:gh+22]
                vld = []
                                        # split string (ttaaii cccc)
                                        # into 2 char pairs
                ghid2 = [ghid[i:i+2] for i in range(0, len(ghid), 2)]
                for xy in range(0, len(ghid2)):
                    vla = ghid2[xy].decode("hex")
                    vld.append(vla)
                vldf = ''.join(vld)
                break
                                        # split string (BUFR => 7777)
                                        # into 2 char pairs
        ghob = [obr[y:y+2] for y in range(0, len(obr), 2)]
        bed = ghob[7]                                                                               
#
        try:                            # BUFR edition no
            bedi = int(bed)
        except:
            belist = errord['1']
            errmf = errm
            errmf[0] = S2
            errmf[1] = str(obn)
            errmf[2] = vldf
            belist.append(errmf)
            errord['1'] = belist
#
                                        # section 1 length 
        try:
            s1len = int(''. join(ghob[8:11]), 16)
        except:
            pflist = errord['12']
            errmf = errm
            errmf[0] = S2
            errmf[1] = str(obn)
            errmf[2] = vldf
            pflist.append(errmf)
            errord['12'] = pflist
            return()
                                        # bufr master table
        bmt = int(''.join(ghob[11:12]), 16)
#
#
                                        #  id generating
                                        # centre and secondary generating
                                        # centre (edition 4)
        if bedi == 4:
            idgc = int(''.join(ghob[12:14]), 16)
            idsgc = int(''.join(ghob[14:16]), 16)
            if str(idgc) in c11_d.keys():
                idgcf = c11_d[str(idgc)]
                idgc = idgcf
            else:
                idgcf = "N/A"
                idgc = idgcf
            if str(idsgc) in c12_d.keys():
                idgscf = c11_d[str(idsgc)]
                idsgc = idsgcf
            else:
                idsgcf = "N/A"
                idsgc = idsgcf
        elif bedi == 3:
                                         #  id generating
                                         # centre and secondary generating
                                         # centre (Edition 3)
            idgc = int(''.join(ghob[12:13]), 16)
            idsgc = int(''.join(ghob[13:14]), 16)
            if str(idgc) in c11_d.keys():
                idgcf = c11_d[str(idgc)]
                idgc = idgcf
            else:
                idgcf = "N/A"
                idgc = idgcf
            if str(idgc) in c12_d.keys():
                idgcf = c11_d[str(idgc)]
                idgc = idgcf
            else:
                idgcf = "N/A"
                idgc = idgcf
#
#                                       
        try:
                                          # BUFR Edition 4 => year
            if bedi == 4:
                byr = int(''.join(ghob[23:25]), 16)
                                         # BUFR Edition 3 => year 
            elif bedi == 3:
                byr = int(''.join(ghob[20:21]), 16)
        except:
            errmf = errm
            pslist = errord['2']
            errmf[0] = S2
            errmf[1] = str(obn)
            errmf[2] = vldf
            errmf[3] = idgc
            pslist.append(errmf)
            errord['12'] = pslist
            continue
#
        try:
                                        # BUFR Edition 4 => month
            if bedi == 4:
                bmth = int(''.join(ghob[25:26]), 16)
                                        # BUFR Edition 3 => month
            elif bedi == 3:
                bmth = int(''.join(ghob[21:22]), 16)
        except:
            errmf = errm
            pslist = errord['2']
            errmt = errm
            errmf[0] = S2
            errmf[1] = str(obn)
            errmf[2] = vldf
            errmf[3] = idgc
            pslist.append(errmf)
            errord['2'] = pslist
            continue
#
        try:
                                        # BUFR Edition 4 => day
            if bedi == 4:
                bdy = int(''.join(ghob[26:27]), 16)   
                                        # BUFR Edition 3 => day
            elif bedi == 3:
                bdy = int(''.join(ghob[22:23]), 16)
        except:
            errmf = errm
            pslist = errord['2']
            errmt = errm
            errmf[0] = S2
            errmf[1] = str(obn)
            errmf[2] = vldf
            errmf[3] = idgc
            pslist.append(errmf)
            errord['2'] = pslist
            continue
#
        try:
                                        # BUFR Edition 4 => hour
            if bedi == 4:
                bhr = int(''.join(ghob[27:28]), 16)
                                        # BUFR Edition 3 => hour
            elif bedi == 3:
                bhr = int(''.join(ghob[23:24]), 16)
        except:
           errmf = errm
           pslist = errord['2']
           errmt = errm
           errmf[0] = S2
           errmf[1] = str(obn)
           errmf[2] = vldf
           errmf[3] = idgc
           pslist.append(errmf)
           errord['2'] = pslist
           continue
#
        try:
                                        # BUFR Edition 4 => minute
            if bedi == 4:
                bmn = int(''.join(ghob[27:28]), 16)
                                        # BUFR Edition 4 => minute
            elif bedi == 3:
                bmn = int(''.join(ghob[23:24]), 16)
        except:
           errmf = errm
           pslist = errord['2']
           errmt = errm
           errmf[0] = S2
           errmf[1] = str(obn)
           errmf[2] = vldf
           errmf[3] = idgc
           pslist.append(errmf)
           errord['2'] = pslist
           continue
#
        combdt = str(byr) + '-' + str(bmth) + '-' + str(bdy) + '-' + ' ' + \
        str(bhr) + ':' + str(bmn)
#
        if bedi == 4:
            s2ind = str(bin(int(ghob[17:18][0], 16))[2:].zfill(8))
        elif bedi == 3:
            s2ind = str(bin(int(ghob[15:16][0], 16))[2:].zfill(8))
        s2pi = s2ind[0]
                                        # gather section 2 length if present
        if s2pi != '0':                                
            s2sp = s1len + 8
            s2len = int(''.join(ghob[s2sp:s2sp +3]), 16)
        else:
            s2len = 0 
                                        # section 3 start point
        s3stp = 8 + s1len + s2len
         # section 3 length
        s3len = int(''.join(ghob[s3stp:s3stp + 3]), 16)
        s3ept = s3len+s3stp
#
        encd = ghob[(s3stp+7):s3ept]    # encoding D sequence
        encl = []
#
        if  len(encd) != 0:
            s3pat = [0, 2, 6]
            fxy = 1
                                        # convert the encoding
                                        # sequence from hex to ascii
            encl = bit_anal(encd, s3pat, fxy)
            uttc = 1
        else:
            uttc = -1
#
        msec = 0
        if opf not in sum_dict.keys():
            sum_dict[opf] = [vldf, bedi, bmt, idgc, idsgc, byr, bmth, bdy, 
                            bhr, bmn, msec, encl]             
    return(sum_dict, errord, uttc) 
#
#############################################################################
#
#
# subroutine : 	    bit_anal
# 
# purpose:          Convert single or multiple HEX value(s) to Binary. Split
#                   the binary string as outlined in bit pattern (bitp) 
#                    and convert the resultant BIN values to integers
# 
# parameters: 
#                   input =>    	
#	            spval -  Input values HEX or Binary 
# 	            bitp  -  bit pattern
#                   fxy   -  indicator
#  
#                   output =>  
#                   flist -  list of converted data
#
#
#############################################################################
#
def bit_anal(spval, bitp, fxy):
    numb = 8
    scale = 16
    olist = []
    flist = []
    lenbp = len(bitp)
    lensp = len(spval)
    for x in range(0, lensp):
        cnv=bin(int(spval[x],scale))[2:]. zfill(numb)                          
                                        # convert the whole of the HEX value 
                                        # to a binary string 
        olist.append(cnv)
    lenbsp = len(olist)
    if fxy :                            # if fxy selected (=TRUE) then 2 bytes 
                                        # will be needed
        for y in range(0, lenbsp):
            yx = 2*y
            if yx < (lenbsp-1):
                val1 = olist[yx]
                val2 = olist[yx+1]
                valf = val1+val2
                fval = str(int((valf[0:2]), 2))
                xval = int((valf[2:8]), 2)
                if xval < 10:
                    xvals = "0"+(str(xval))
                else:
                    xvals = str(xval)
                yval = int((valf[8:]), 2)
                if yval < 100:
                    if yval < 10:
                        yvals = "00"+(str(yval))
                    else:
                        yvals = "0"+(str(yval))
                else:
                    yvals = str(yval)
                fxyval = fval+xvals+yvals
                flist.append(fxyval)
    else:                               # assumes a single binary string
        for y in range(0, lenbp):
            if y < (lenbp-1):
                valf = olist[0]
                rn1 = bitp[y]
                rn2 = bitp[y+1]
                val = str(int((valf[rn1:rn2]), 2))
                flist.append(val)
            else:
                rn1 = bitp[y]
                val = str(int((valf[rn1:]), 2))
                flist.append(val)

 # 
    return(flist)
#   
#
#############################################################################
#
#
# subroutine : 	    read_c11
#
# purpose:         Read in common code table C11 to a dict of lists
#
# parameters:
#                   input =>
#                   file_in - Filename common code table C11
#                   The format of the entries in CC C-11 are as follows - 
#                   Entry No, 
#                   CREX (B001035) entry
#                   BUFR (001035) entry  
#                   Code of OriginatingCentres",
 
#                   output
#                   c11_read - dict output
#
#
#############################################################################
#
def read_c11(file_in):
    c11_read = {}
    rg = "Region"
    with open(file_in,'r') as fb:
        fb.next()
        fb.next()
        fb.next()
        for line in fb:
            stringtf = line.replace(";", " ")
            stringtf = line.replace(",", ";")
            stringtf = stringtf.replace("\","," ")  
            stringt = stringtf. split(";")
            test_st = stringt[1]
            test_l = len(stringt)
            tno = test_st.find("rg")
            if tno == -1 and test_l > 4:
                c11_code = stringt[2]
                ent = stringt[3]
                c11_read[c11_code] = ent
    return(c11_read)
#
#############################################################################
#
#
# subroutine : 	    read_c12
#
# purpose:         Read in common code table C12 to a dict of lists
#
# parameters:
#                   input =>
#                   file_in2 - Filename common code table C12
#                   Note:
#                   The format of the entries in CC C-12 are as follows - 
#                   No, 
#                   Code of OriginatingCentres",
#                   Name OriginatingCentres_en,
#                   CodeFigure_SubCentres,
#                   Name_SubCentres_en,
#                   Status   
#
#                   output
#                   c11_read - dict output each key being of the form 
#                    [['240', 'Tokyo (RSMC)  Japan Meteorological Agency', '
#                    Kiyose']]     
#
#############################################################################
#
def read_c12(file_in2):
    c12_read = {}
    templ = []
    ml = []
    rg = "Region"
    with open(file_in2, 'r') as fb:
        fb.next()
        fb.next()
        fb.next()
        fb.next()
        fb.next()
        for line in fb:  
            stringtf = line.replace(";", " ")
            stringtf = line.replace(",", ";")
            stringtf = stringtf.replace("\","," ")   
            stringt = stringtf.split(";")
            test_st = stringt[2]
            tno = test_st.find(rg)
            if tno == -1:
                c12_code = stringt[1]
                mcent = stringt[2]
                scentno = stringt[3]
                scent = stringt[4]
                templ = []
                templ.append(scentno)
                templ.append(mcent)
                templ.append(scent)
                if c12_code in c12_read.keys():
                    ml = c12_read[c12_code]
                    ml.append(templ)
                    c12_read[c12_code] = ml 
                else:
                    t2 = []
                    t2.append(templ)
                    c12_read[c12_code] = ml  
#
    return(c12_read)
#
#############################################################################
#
# subroutine name:  listing_build
#
# purpose:          Construct listing of all files in the processed
#                   directory matching the criteria set out in the
#                   config file and according to the time limitations.           
#
# input:           ah    :  Criteria set out in the configuration file_in
#                           presented as a list
#                  tsp1  :  no of minutes. This is used to calculate the span
#                           of the file listing
#
# output:          flist :  list of files fitting the time and char
#                           matching criteria
#
#############################################################################
#
def listing_build(ah,tsp1):
#
    ah2=ah[5]                                   # extract dir path for the files 
    now = dt.datetime.now()
    ago = now-dt.timedelta(minutes=tsp1)
    flist = []
    for root, dirs, files in os.walk(ah2):       # generate list of filenames
                                                # from the processed directory.
        for fname in files:
            path = os.path.join(root, fname)
            st = os.stat(path)
            mtime = dt.datetime.fromtimestamp(st.st_mtime)
            if mtime > ago:
                flist.append(path)
    return(flist)
#
#############################################################################
#
# subroutine name:  fprocess
#
# purpose:          Decode and construct listing of sections 1, 2 & 3
#                   of each MHSR* file listed by listing_build.
#
#
# input:           indct    :  Dictionary of lists of the form :-
#                              [['Datatype','storage_datasets','header file'
#                              ,'elements_index','processed Directory',
#                              [[element index 1[,]element index 2],[.....]]
#                              ,[MHSR file listing]]]
#                              defined sequences.
#                  c11_dict :  Dictionary of table C11 values
#                  c12_dict :  Dictionary of table C12 values
#                  errpf    :  Dictionary of encoding / decoding errors
#                              (presented as a list)
#                  hdrrng   :  TTAAii header ranges for that data type (HEX)
#
# calls:           sum_bin
#
# output:          brdict3 :  Dictionary of lists:-
#                             MHSR FILENAME - Bulletin-No{['TTAAii CCCC','
#                             BUFR Edition','BUFR Master Table No',
#                             'Originating Centre','Originating Sub-Centre',
#                             'Year','Month','Day','Hour','Minute','Second'
#                             ,['Encoding D Sequence']]
#                  example =>
#                  /var/moods/bulletins/processed/SNT1/
#                  MHSR.R2D17236.T091005.SNT1.s450-12
#                 ['IOPK01 AMMC', 4, 0, 'Melbourne', 'N/A', 2017, 8, 8, 4,
#                 40, 0, ['315003']]
#
#############################################################################
#
def fprocess(indct, c11_dict, c12_dict):
#
    brdict = {}
    brdict2 = {}
    brdict3 = {}
    nm = {}
    #
    # errw => intialise errw dictionary to capture decoding errors
    #
    errw = {}
    errw['1'] = [['Unable to find BUFR Edition']]
    errw['2'] = [['Binary Read error in aquiring date - Time']]
    errw['3'] = [['Binary Read error in calculating no of data subsets']]
    errw['4'] = [['FXY Encoding sequence unobtainable ']]
    errw['5'] = [['Unidentified D sequence in Decode of MHSR file']]
    errw['6'] = [['Unidentified D sequence in Decode of Element Index']]
    errw['7'] = [['Incorrectly encoded value for section 3 length ' \
                 '(section 2 not present) -  BUFR Edition 3']]
    errw['8'] = [['Incorrectly encoded value for section 3 length ' \
                 '(section 2 present) - BUFR Edition 3']]
    errw['9'] = [['Incorrectly encoded value for section 3 length ' \
                   '(section 2 not present) - BUFR Edition 4']]
    errw['10'] = [['Incorrectly encoded value for section 3 length ' \
                    '(section 2 present) - BUFR Edition 4']]
    errw['11'] = [['TTAAii not in prescibed header range ']]
    errw['12'] = [['Incorrectly encoded value for section 1 length ']]
    errw['13'] = [['Unable to find BUFR in message ']]
#
    nrrf = -1
#    
     
    for mik in indct.keys():
        dty = indct[mik][0][0]          # data type            
        dataf = indct[mik][0][8]        # last entry in the list will be a        
                                        # listing of the MHSR data associated
                                        # with the header ranges etc.
        hddata = indct[mik][0][7]       # header ranges in hex
        hrange = indct[mik][0][6]       # element index sequences

        if len(dataf) == 0:
            break
        else:
            for filein in dataf:
                if len((filein.strip())) == 0:
                    break                           
                else:
                    (brdict, errpf, nrrf) = sum_bin(filein, c11_dict, c12_dict, errw) 
                    if nrrf > -1:
                        for mib in brdict.keys():
                            tlist = brdict[mib]
                            tlist.insert(0, dty)
                            tlist.append(hddata)
                            tlist.append(hrange)
                            brdict2[mib] = tlist
                            tlist = []                     
    for mib2 in brdict2.keys():          # brdict contains a decode of the
                                        # MHSR BUFR file headers, 1 per
                                        # message ending with the BUFR
                                        # encoding sequence
        qlist = brdict2[mib2]
        key2 = mib2.split("/")
        key3 = key2[-1:][0]                     
        brdict3[key3] = qlist           # replace the dict entry with
                                        # the new list, which includes
                                        # the expanded encoding sequence    
    for mib3 in brdict3.keys():
        idh = brdict3[mib3][1].split(' ')
        idhf = idh[0]
        hident = binascii.b2a_hex(idhf)
        hident1 = bin(int(hident, 16))[2:]
        hidentf = int(hident1, 2)
        hrange = brdict3[mib3][13]
        hrlen = len(hrange)
        passh = 0
        for xh in range(0, hrlen):
            trange = hrange[xh]
            if hidentf >= trange[0] and hidentf <= trange[1]:
                passh = 1
                break
        if passh == 0:
            errc = 11
            key2 = mib3.split('/')
            key3 = key2[-1:][0].split('-')
            keyf = key3[0]
            obn = key3[1]
            ttaaii = brdict3[mib3][1]
            dlist = errw['11']
            dlist2 = []
            dlist2.append(keyf)
            dlist2.append(obn)
            dlist2.append(ttaaii)
            dlist.append(dlist2)
            errw['11'] = dlist
            brdict3.pop(mib3, None)
#
    nm = {}      
    for mib4 in brdict3.keys():             # iterate through the bull entries
        emcseq = brdict3[mib4][13]          # establish the bull encoding seq
        if len(emcseq) == 0:
            tlx = errw['4']
            key6 = mib4.split('/')
            key7 = key6[-1:][0].split('-')
            key7f = key7[0]
            obn = key7[1]
            ttaaii = brdict3[mib4][1]
            dlist3 = []
            dlist3.append(key7f)
            dlist3.append(obn)
            dlist3.append(ttaaii)
            tlx.append(dlist3)
            errpf['4'] = tlx
            continue
        lenenc = len(emcseq)             # length of the encoding sequence
        elseq = brdict3[mib4][14]        # expanded retrival element indexes
                                         # (may be 1+)
        lenel = len(elseq)               # no of entries in the element
                                         # index list of lists
        tlap = []
        tlap2 = []
        tlap3 = []
#
    for mib5 in brdict3.keys():
        tlist = brdict3[mib5][14]
        lentl = len(tlist)
        for xp in range(0, lentl):
            tlist2 = tlist[xp]
            lentl2 = len(tlist2)
            alist = []
            for xg in range(0, lentl2):
                if tlist2[xg].isalnum():
                    alist.append(tlist2[xg])
            tlist[xp] =  alist
        brdict3[mib5][14] = tlist
#
    for mib6 in brdict3.keys():
        f1list = brdict3[mib6][12]       # bulletin BUFR encoding sequence
        f2list = brdict3[mib6][14]       # retrieval index sequences
                                         # (nested lists)
        lenel = len(f2list)
        for zx in range(0, lenel):
            eh = [(i, j) for i, j in zip(f2list[zx], f1list) if j != i]
            if len(eh) == 0:
                break                   # len(eh) == 0 exact match
                                        # bewteen the encoding seq
                                        # and the index
            elif zx == lenel -1 and len(eh) > 0:
                nm[mib6] = brdict3[mib6]
#     
    return(nm, errpf)
#
#############################################################################
#
#
# subroutine name: read_config
#
# purpose:         read config file. Output details to a dict of lists.
#
# input:           cnfgin     :      config file name
#
#
# output:          fdict      :      output dictionary of lists of the form
#                                    odict[job stream]{[['data type',
#                                    'storage_datasets','header_file',
#                                    'element_index',
#                                     bufr_localSeq,'processed dir],[..]]}
#
#############################################################################
#
def read_config(cnfgin, trigger):
    olist = []
    fdict = {}
    TFIG = 'NOW SERVING => '
    with open(cnfgin, 'r') as fg:
        for line in fg:   
            if line.find(trigger) >= 0:
                fields=line.split()
                keyf=fields[-1].split('/')[-2]
                if keyf in fdict.keys():
                    tlist=fdict[keyf]
                    tlist.append(fields[1:])
                    fdict[keyf]= tlist
                else:
                    tlist = []
                    tlist.append(fields[1:])
                    fdict[keyf] = tlist

        fg.close()
    return(fdict)
#
#############################################################################
#
# subroutine name: web_dout
#
# purpose:         Output results to a html page.
#
# input:           wcd1     :  Dictionary of lists for incoming D
#                              sequences which do not have an equivalent
#                              index entry
#
#                  wcd2     :  Dictionary of lists containg error messages.
#
# output:          write out to D_sequence_comparison.html
#
#
#############################################################################
#
def web_dout(wcd1, wcd2, wdout):
#
    fdx = wdout + '/D_sequence_comparison.html'
    with open(fdx, 'w') as fxw:
        fxw.write(('%s\n') % ("<!DOCTYPE html>"))
        fxw.write(('%s\n') % ("<html>"))
        fxw.write(('%s\n') % ("<head>"))
        fxw.write(('%s\n') % ("<style>"))
        fxw.write(('%s\n') % ("table, th, td {"))
        fxw.write(('%s\n') % ("border: 1px solid white;"))
        fxw.write(('%s\n') % ("border-collapse: collapse;"))
        fxw.write(('%s\n') % ("}"))
        fxw.write(('%s\n') % ("th, td {"))
        fxw.write(('%s\n') % ("padding: 5px;"))
        fxw.write(('%s\n') % ("}"))
        fxw.write(('%s\n') % ("</style>"))
        fxw.write(('%s\n') % ("</head>"))
        fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
        fxw.write(('%s\n') % ("<caption style=\"background-color:#ebfafa\">"\
                   "<b>Data rejected with no matching Element Index </b>" \
                   "</caption>" ))
        fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\">"\
                   "<b> MHSR file name </b></td>"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"5%\">"\
                "<b>Ob No </b></td>"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"15%\" >"\
                 "<b>TTAAii CCCC </b></td>"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"15%\" > "\
                 "<b>YYYY-MM-DD hh:mm:ss</b> </td>"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"25%\"> "\
                 "<b>Originating Centre </b></td>"))
        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\" > "\
                 "<b>FXY (Encoding Sequence) </b></td>"))
        fxw.write(('%s\n') % ("</tr>"))
        n = 1
        for wky in wcd1.keys():
            if n% 2 == 0:
                fxw.write(('%s\n') % ("<tr style=\"background-color:"\
                           "#e6fff7\" width=\"100%\" >"))
            else:
                fxw.write(('%s\n') % ("<tr width=\"100%\">"))
            n = n+1
            linew=wcd1[wky]
            tai = linew[1]  
            kout=wky.split("/")           
            kf=kout[-1:][0]
            kf2=kf.split("-")
            obn=kf2[1]
            kff=kf2[0]
            ens = " ".join(map(str,linew[12]))
            ens2 = ens+'<br>' 
             
            mnf = str(linew[7])
                 
            dyf=str(linew[8])
            
            hrf=str(linew[9])
                
            mnt=str(linew[10])

            scf = "00"
                
            hms = (str(hrf)).zfill(2)+":"+(str(mnt)).zfill(2)+":"+(str(scf)).zfill(2)   
            dmd = str(linew[6])+"-"+(str(mnf)).zfill(2)+"-"+(str(dyf)).zfill(2)    
                      
            ogc= linew[4]

            el1="<td style=\"font-size:17px\" width=\"20%\" >" \
                 +str(kff)+"</td>" 
            fxw.write(('%s\n') % (el1))

            obo="<td style=\"font-size:17px\" width=\"5%\" >" \
                 +obn+"</td>"
            fxw.write(('%s\n') % (obo))

            taio="<td style=\"font-size:17px\" width=\"15%\">" \
                 +tai+"</td>"
            fxw.write(('%s\n') % (taio))

            fdm = "<td style=\"font-size:17px\" width=\"15%\" >" \
                 +dmd+" "+hms+"</td>"
            fxw.write(('%s\n') % (fdm))

            ogcm = "<td style=\"font-size:17px\" width=\"25%\" >" \
                 +ogc+"</td>"
            fxw.write(('%s\n') % (ogcm))
             
            encf="<td style=\"font-size:17px\" width=\"20%\">" \
                 +ens2+"</td>"
            fxw.write(('%s\n') % (encf))

            fxw.write(('%s\n') % ("</tr>"))
        fxw.write(('%s\n') % ("</table>"))
#
        fxw.write(('%s\n') % ("<br>"))
#
        fxw.write(('%s\n') % ("<style>"))
        fxw.write(('%s\n') % ("table, th, td {"))
        fxw.write(('%s\n') % ("border: 1px solid black;"))
        fxw.write(('%s\n') % ("border-collapse: collapse;"))
        fxw.write(('%s\n') % ("}"))
        fxw.write(('%s\n') % ("th, td {"))
        fxw.write(('%s\n') % ("padding: 5px;"))
        fxw.write(('%s\n') % ("}"))
        fxw.write(('%s\n') % ("</style>"))
        fxw.write(('%s\n') % ("</head>"))
        fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
        fxw.write(('%s\n') %  ("<caption style=\"background-color:" \
                   "#ebfafa\"><b> Decoding Errors </b></caption>"))
        fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
        fxw.write(('%s\n') % ("</table >"))
        fxw.write(('%s\n') % ("<br>"))
#
        for key in wcd2.keys():
            if (key == '1' and len(wcd2['1']) > 1):
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                          "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                       "colspan=\"3\"><b> Error in Read of BUFR Edition "\
                        "</b></td>"))
                fxw.write(('%s\n') % ("</tr>"))
                fxw.write(('%s\n') % ("</table>"))

                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color" \
                            ":#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                         "width=\"35%\"><b> MHSR file name </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"35%\"><b> TTAAii CCCC </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                            "width=\"10%\"><b> Ob No </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))

                for y in range(0, len(wcd2['1'])): 
                    mhsr = "<td style=\"font-size:17px\" width=\"35%\" >" \
                           +wcd2[key][y][1]+"</td>"
                    obfn = "<td style=\"font-size:17px\" width=\"10%\" >" \
                           +wcd2[key][y][2]+"</td>"
                    ttaan = "<td style=\"font-size:17px\" width=\"20%\" >" \
                           +wcd2[key][y][3]+"</td>"
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (obfn)) 
                    fxw.write(('%s\n') % (ttaan))
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))  

            elif  (key == '2' and len(wcd2['2']) > 1):   
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                            "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\"  " \
                           "colspan=\"4\"><b> Error in Read Date / Time " \
                           " </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))
                fxw.write(('%s\n') % ("</table>"))
#                
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                             "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                             "width=\"35%\"><b> MHSR file name </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              "width=\"15%\"><b> TTAAii CCCC </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                               "width=\"20%\"><b> Ob No </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\"" \
                               "width=\"30%\"><b> Originating Centre </b>" \
                                "</td>"))
                fxw.write(('%s\n') % ("</tr>"))     
                for y in range(0, len(wcd2['2'])): 
                    mhsr = "<td style=\"font-size:17px\" width=\"35%\" >" \
                            +wcd2[key][y][1]+"</td>"
                    ttaan = "<td style=\"font-size:17px\" width=\"20%\" >" \
                            +wcd2[key][y][2]+"</td>"
                    obfn = "<td style=\"font-size:17px\" width=\"15%\" >" \
                            +wcd2[key][y][3]+"</td>"
                    ocr = "<td style=\"font-size:17px\" width=\"30%\" >" \
                            +wcd2[key][y][4]+"</td>"
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (ttaan))
                    fxw.write(('%s\n') % (obfn))
                    fxw.write(('%s\n') % (ocr))
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))
            elif (key == '3' and len(wcd2['3']) > 1):
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                            " #ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "colspan=\"4\"><b> Error in Read " \
                            "of No od data subsets </b></td>")) 
                fxw.write(('%s\n') % ("</tr>"))          
                fxw.write(('%s\n') % ("</table>")) 

                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                          "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"35%\"><b> MHSR file name </b></td>"))   
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"15%\"><b> TTAAii CCCC </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"10%\"><b> Ob No </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"10%\"><b> Date / Time</b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                           "width=\"30%\"><b> Originating Centre </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))     
                for y in range(0, len(wcd2['3'])): 
                    mhsr = "<td style=\"font-size:17px\" width=\"35%\" >" \
                            +wcd2[key][y][1]+"</td>"
                    ttaan = "<td style=\"font-size:17px\" width=\"20%\" >" \
                            +wcd2[key][y][2]+"</td>"
                    obfn = "<td style=\"font-size:17px\" width=\"15%\" >" \
                             +wcd2[key][y][3]+"</td>"
                    dtim = "<td style=\"font-size:17px\" width=\"20%\" >" \
                             +wcd2[key][y][4]+"</td>"
                    ocr = "<td style=\"font-size:17px\" width=\"20%\" >" \
                             +wcd2[key][y][5]+"</td>"
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (ttaan))
                    fxw.write(('%s\n') % (obfn))
                    fxw.write(('%s\n') % (ocr))
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))
            elif  (key == '4' and len(wcd2['4']) > 1) or (key == '5' and \
                      len(wcd2['5']) > 1) :
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                          "#ebfafa\">"))
                if key == '4':
                    ltr = len(wcd2['4'])
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              "colspan=\"4\"><b> FXY Encoding sequence " \
                               "unobtainable </b></td>"))
                else:
                    ltr = len(wcd2['5'])
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              "colspan=\"4\"><b> Unidentified D sequence in" \
                              "Decode of MHSR file </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))       
                fxw.write(('%s\n') % ("</table>"))   
   
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                           "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                          "width=\"35%\"><b> MHSR file name </b></td>"))  
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                          "width=\"15%\"><b> TTAAii CCCC </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" "
                          "width=\"10%\"><b> Ob No </b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                          "width=\"10%\"><b> Date / Time</b></td>"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                          "width=\"30%\"><b> Originating Centre </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))
                     
                for y in range(0,ltr ):
                    mhsr = "<td style=\"font-size:17px\" width=\"35%\" >" \
                           +wcd2[key][y][0]+"</td>"
                    ttaan = "<td style=\"font-size:17px\" width=\"20%\" >" \
                           +wcd2[key][y][1]+"</td>"
                    obfn = "<td style=\"font-size:17px\" width=\"15%\" >" \
                           +wcd2[key][y][2]+"</td>"
                    dtim = "<td style=\"font-size:17px\" width=\"20%\" >" \
                           +wcd2[key][y][3]+"</td>"
                    ocr = "<td style=\"font-size:17px\" width=\"20%\" >" \
                           +wcd2[key][y][4]+"</td>"
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (ttaan)) 
                    fxw.write(('%s\n') % (obfn)) 
                    fxw.write(('%s\n') % (ocr))
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))
            elif  (key == '7' and len(wcd2['7']) > 1)  \
                or (key == '8' and len(wcd2['8']) > 1) \
                or (key == '9' and len(wcd2['9']) > 1)  \
                or (key == '10' and len(wcd2['10']) > 1) \
                or (key == '11' and len(wcd2['11']) > 1):
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                           " #ebfafa\">"))  
                if key == '7' :
                    ltr = len(wcd2['7'])
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                             "colspan=\"3\"><b> Incorrectly encoded value " \
                             " for section 3 length (section 2 not present)" \
                              " -  BUFR Edition 3 </b></td>")) 
                elif key == '8' :
                    ltr = len(wcd2['8']) 
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              "colspan=\"3\"><b> Incorrectly encoded value " \
                              " for section 3 length (section 2 present) - " \
                               "BUFR Edition 3</b></td>"))
                elif key == '9' :
                    ltr = len(wcd2['9'])
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                             " colspan=\"3\"><b> Incorrectly encoded value " \
                             " for section 3 length (section 2 not present) " \
                              "- BUFR Edition 4 </b></td>"))
                elif key == '10' :
                    ltr = len(wcd2['10'])
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              " colspan=\"3\"><b> Incorrectly encoded value " \
                               " for section 3 length (section 2 present) - " \
                               " BUFR Edition 4 </b></td>"))
                else:
                    ltr = len(wcd2['11']) 
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                              " colspan=\"3\"><b> TTAAii not in prescibed " \
                               " header range</b></td>")) 
                fxw.write(('%s\n') % ("</tr>"))    
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))
    
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:" \
                           "#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                          " width=\"35%\"><b> MHSR file name </b></td>"))  
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                            "width=\"10%\"><b> Ob No </b></td>"))  
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\"" \
                            "width=\"15%\"><b> TTAAii CCCC </b></td>"))    
                fxw.write(('%s\n') % ("</tr>"))           
                for y in range(1,ltr ): 
                    mhsr = "<td style=\"font-size:17px\" " \
                           "width=\"35%\" >"+wcd2[key][y][0]+"</td>" 
                    obfn = "<td style=\"font-size:17px\" width=\"15%\" >" \
                            +wcd2[key][y][1]+"</td>"   
                    ttaan = "<td style=\"font-size:17px\" width=\"20%\" >" \
                            +wcd2[key][y][2]+"</td>"                        
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (obfn))
                    fxw.write(('%s\n') % (ttaan)) 
 
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))
            if (key == '12' and len(wcd2['12']) > 1):
                fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                            "width=\"35%\"><b> MHSR file name </b></td>"))   
                fxw.write(('%s\n') % ("<td style=\"font-size:17px\" " \
                             "width=\"35%\"><b> TTAAii CCCC </b></td>"))
                fxw.write(('%s\n') % ("</tr>"))     
                for y in range(0,len(wcd2['12'])): 
                    mhsr="<td style=\"font-size:17px\" width=\"35%\" >" \
                          +wcd2[key][y][0]+"</td>" 
                    ttaan="<td style=\"font-size:17px\" width=\"10%\" >" \
                          +wcd2[key][y][1]+"</td>"   
                    fxw.write(('%s\n') % ("<tr>"))
                    fxw.write(('%s\n') % (mhsr))
                    fxw.write(('%s\n') % (ttaan))
                fxw.write(('%s\n') % ("</table>"))
                fxw.write(('%s\n') % ("<br>"))  
          
        fxw.close()  
    return() 
#
#############################################################################
#
# subroutine name: read_hdr
#
# purpose:                Read the First and Last TTAAii from
#                         the header file for that data type.
#                         Convert the TTAAii to HEX values
#
# input:                  def_file -  header file name
#                         sname    -  data type
#
#
# output:                lflist => [[TTAAii(start) (HEX) ,
#                        TTAAii(end) (HEX)], [TTAAii(start) (HEX) ,
#                        TTAAii(end) (HEX)],[TTAAii(start) (HEX) ,
#                        TTAAii(end) (HEX)].....]
#
#
#############################################################################
#
def read_hdr(def_file,sname):
#   
    lslist=[]  
    lflist=[] 
    with open(def_file,'r') as fd:
        for x in range(0,7):
            line=fd.next() 
        for line in fd:
            linesub=line[0:24]
            line2=linesub.split()            
#             
            if len(line2) != 0:
                if line2[2] == sname:
                    rng1=line2[0]
                    hrng1=binascii.b2a_hex(rng1)
                    brng1=bin(int(hrng1,16))[2:]
                    inrng1=int(brng1,2)
#                   
                    rng2=line2[1]
                    hrng2=binascii.b2a_hex(rng2)
                    brng2=bin(int(hrng2,16))[2:]
                    inrng2=int(brng2,2)
#
                    lslist.append(inrng1)
                    lslist.append(inrng2) 
                    lflist.append(lslist)
                    lslist=[]                                                                    
            else:
                break      
    return(lflist)
#
#############################################################################
#
# Main calling function
#
# create listing of the directorys / files needed to proceed.
#
# MHSR files in data source directory
#
#
#############################################################################
#
#
cfin='config_table_NEW1.txt'
#
osin = os.environ['MACHINE']
wout = os.environ['WEB_HTML_DIR']
cnfg=read_config(cfin,osin)  
#
elist=[]                                # Temporary list (position of elements
                                        # index                                   
brdictxf={}                             # Final out put dictionary, will 
                                        # contain processed data ready to be 
#                                       # output by web_dout        
# 

for key in cnfg.keys():    
    tdir =cnfg[key][0][4]               # definition of the tables directory
                                        # from the config file  
    break                               # taken on first loop 
#
mldir = os.listdir(tdir)                # listing of the tables dir (tdir)
#                                                                                    
lenml = len(mldir)
#
for x in range(0,lenml):                                             
                                        # establish location and name of 
                                        # common code tables C11 & C12  
    if mldir[x].find("Common_C11") > -1:                              
                                        # these are required in decoding 
                                        # to identify issuing centres 
        c11_file=tdir+mldir[x]
        (c11_dict)=read_c11(c11_file)      
                                        # Read file C11 into dict  
    elif mldir[x].find("Common_C12") > -1:
        c12_file=tdir+mldir[x]
        (c12_dict)=read_c12(c12_file)                                  
                                        # Read file C12 into dict   
#
#############################################################################
#
xp=60                                  # constant (in mins) used to 
                                        # calculate the 
                                        # time span of the data to be 
                                        # reviewed .
plist = []
hdict = {}
                                        # add header file ranges and 
                                        # MHSR listing to the cnfg entry
                                        # for that data type.
                                        # entries in the cnfg dict will then be 
                                        # cnfg = [ header file,'storage_datasets'
                                        # elements_index,processed directory, 
                                        # header file ranges (HEX),
                                        # MHSR file listing] 
for cn1 in cnfg.keys():                                             
    df = cnfg[cn1]
    ldf = len(df)
    for yx in range(0, ldf):                                      
                                        # compile listing of data in dir 
                                        # and header file in HEX     
        hdn=df[yx][1]
        din=df[yx]    
        sn = df[yx][0]
        slist = listing_build(din, xp)  # take listing of the MHSR files 
                                        # fitting the date / time criteria
        plist = read_hdr(hdn, sn) 
        df[yx].append(plist)
        df[yx].append(slist)                                     
    cnfg[cn1]=df                      
for cn2 in cnfg.keys():
    dfl = len(cnfg[cn2])
    for xv in range(0,dfl):
        ein = cnfg[cn2][xv] 
        dtype = cnfg[cn2][xv][0]        # data type name 
        elist = cnfg[cn2][xv][3]        # name and position of elements index 
        elout = ind_read(elist)         # read in retrieval index(s) for a 
                                        # specific data type   
        ein.insert(6,elout)             # add entry to cnfg entry for that 
                                        # data type 
        cnfg[cn2][xv] = ein 
#
eldict2 = {}
#
# 
# 
(brdictxf,erryx) = fprocess(cnfg, c11_dict, c12_dict)                   
                                        # final processing of the data  
                                        # including decode of the MHSR 
                                        # data
#
web_dout(brdictxf, erryx, wout)                                            
#
#
#############################################################################
#