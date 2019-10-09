# -*- coding: iso-8859-1 -*-
import sys
import binascii
import itertools
import operator
import collections
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
#             
        flist2 = flist[1:]
        flist2x = []
        for xz in range(0,len(flist2)):
           xv = flist2[xz]
           xvx = []
           for xp in range(0,len(xv)):               
              if xv[xp].isdigit():
                 xvx.append(xv[xp])
           flist2x.append(xvx)          
    fi.close()
    return(flist2x)
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
#                   
#	            input  => S2      -  full filename of the MHSR file.
#
#                   output => mdict - dict containing Decoded (bin - ascii)
#                                        data from sections 1 - 3 of the MHSR
#                                        file.
#                             errd     - Error dictionary              
#
#
#############################################################################
#
#
def sum_bin(S2,errd,dtp):
#
    ed42 = [0,8,14,16,22,24,28,32,34,36,38,40,42,44,46,50,52,54,56,58,60]
    ted  = ['BUFR','Total Length','Ed No','S1 Length','Master Table', \
            'ID1 ','ID2 ','Seq No','S2 Ind ','Data Cat ','Sub Cat',\
            'Local Sub Cat','Master Table','Local','Year','Month','Day',\
            'Hour ','Min ','Sec ','Section 3 start point','Section 3 Length']         
    ed32 = [0,8,14,16,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50]
    pattern = "42554652"      # hex(BUFR)
    pattern2 = "37373737"     # hex(7777)
    recogp = [["494F42"],["494F50"],["495341"],["495349"],["49534D"],["49534E"],["495343"], \
              ["495353"],["495541"],["49554A"],["49554B"],["494F53"],["495553"],["495557"]]

#
    rdict = {}
    p3val = []
    p4val = []
    tstring = ''
    sum_dict = {}
    mdict = {}
#   construct list containing title & coordinates within the 
#   data array for BUFR edition 4     
    nx = 0 
    for xz in range(0,len(ed42)):
       if xz <= len(ed42)-2:
          dval = []           
          val1 = ed42[xz]
          val2 = ed42[xz+1]
          dval .append(ted[nx])
          dval.append(val1)
          dval.append(val2) 
          p4val.append(dval)
          nx += 1 
#   construct list containing title & coordinates within the 
#   data array for BUFR edition 3 
    ny = 0
    for xz in range(0,len(ed32)):
       if xz <= len(ed32)-2:
          dval = []           
          val1 = ed32[xz]
          val2 = ed32[xz+1]
          dval .append(ted[ny]) 
          dval.append(val1)
          dval.append(val2) 
          p3val.append(dval)
          ny += 1       
#
    rgx = re.compile(pattern)
    rgx2 = re.compile(pattern2)
    s2f = S2.replace('//','/')
    s2fr = s2f.split('/')    
    sf = s2fr[-1:][0]
    sfd = dtp
    sfd2 = sf + '-' + dtp
    
#
# Read binary file into HEX array (data2)
#
    with open(S2, 'rb') as fp:
        data = fp.read()
        data2 = binascii.b2a_hex(data)
    fp.close()
#
#  
# search data2 for "42554652" hex  (BUFR)
# output start and end positions as nested lists
# within list n2list
#
    n1list = []
    n2list = []
    for match_obj in rgx.finditer(data2):
       st1 = match_obj.start()
       st2 = match_obj.end() 
       n1list.append(st1)
       n1list.append(st2)
       n2list.append(n1list)
       n1list = []
#
# search binary array for "37373737" hex  (7777)
# output start and end positions as nested lists
# within list n12list
#
    n12list = []
    n22list = []
    for match_obj2 in rgx2.finditer(data2):
       st12 = match_obj2.start()
       st22 = match_obj2.end() 
       n12list.append(st12)
       n12list.append(st22)
       n22list.append(n12list)
       n12list = [] 
#
    if len(n2list) != len(n22list) or len(n2list) == 0 or len(n22list) == 0 :    
       ederr = errd['2'][1]
       ederr.append(sfd2)
       errd['2'][1] = ederr
       return(mdict,errd)
#
# combine the coordinate lists from the previous steps to 
# pairs , start and end of a bulletin with the binary array
# within list bdet2
#
    bdet2 = []
    if len(n2list) == len(n22list):
       for xp in range(0,len(n2list)):
          start1 = n2list[xp][0]
          end1 = n22list[xp][1]
          bdet = []
          bdet.append(start1)
          bdet.append(end1)
          bdet2.append(bdet) 
#
# flatten the list bdet2 (remove nested listing)
    fltp = [item for sublist in bdet2 for item in sublist]            
#
    flt2 = fltp[:-1]
#
    flt2.insert(0,0)
#
    dlist2 = []
#
# manipulate the flattened list to reflect the start 
# and end of the bulletins and the gaps in between. 
# This is needed to extract the TTAAii CCCC
# output to list dlist2 as nested lists
#
    for xp in range(0,len(flt2),2):
       dlist = []
       dlist.append(flt2[xp])
       xp += 1 
       dlist.append(flt2[xp])
       dlist2.append(dlist)
#
# extract the TTAAII CCCC for each message and output 
# to list gtsid
#
    gtsid = []  
    gtm = [] 
    gtm2 = [] 
    for df in range(0,len(dlist2)):
       tdx = dlist2[df]
       tcd1 = tdx[0] 
       tcd2 = tdx[1] 
       tstr = data2[tcd1:tcd2]
       tstr2 =tstr.upper()
       gtm.append(tstr2)
       gtm2.append(gtm)
       gtm = []
#
    for xt in range(0,len(gtm2)):
       gtest = gtm2[xt][0]
       ntp = -1         
       for dz in range(0,len(recogp)):
          tfd = gtest.find(recogp[dz][0])
          if tfd > -1:    # found ttaaii
             decog = gtest[tfd:tfd+22]                 
             id1 = decog.decode("hex")                
             gtsid.append(id1)   
             ntp = 1                                    
             break 
       if ntp > -1:
          break       
    if ntp == -1:
       sf2d = sf + '-' + str(df) + '-' + sfd   
       erlt = errd['4'][1]            
       erlt.append(sf2d) 
       errd['4'][1] = erlt   

    dhlist = []
    dshlist = []
    nob = 1
    if len(bdet2) == len(gtsid) :
       for df in range(0,len(bdet2)):
          cd1 = bdet2[df][0]
          cd2 = bdet2[df][1]
          idn = gtsid[df]       
          trange = data2[cd1:cd2]          
          dhlist.append(nob)        # ob no
          dhlist.append(idn)        # TTAAii CCCC
          dhlist.append(trange)     # hex dump of ob 'BUFR' => '7777' 
          S3 = S2.split('/')
          S3_f = S3[-1:][0]
          dhlist.append(S3_f)       # MHSR....s0...        
          dshlist.append(dhlist)                 
          dhlist = []   
          nob += 1

#
    mlist = []  
    mdict = {}   
    for xd in range(0,len(dshlist)):
       tnob = dshlist[xd][0]
       tid = dshlist[xd][1]
       tlist = dshlist[xd][2]
       mfile = dshlist[xd][3]
       BFed = int(tlist[14:16],16)            # BUFR Edition      
       if BFed == 4:
          refl = p4val
       elif BFed == 3:
          refl = p3val
       else:
          ederr = errd['3'][1]
          sed = sf + '-' + str(xd) + '-' + sfd
          ederr.append(sed)
          errd['3'][1] = ederr  
#
       mlist = []
       dfout = []
       dfout.append(tid)  
       mlist.append(dfout)
#
       for xp in range(0,len(refl)):          # loop through index
          xval = refl[xp][1]                  # start index val               
          zval = refl[xp][2]                  # end index val
          sref = refl[xp][0]                  # section title 
          dcval = int(tlist[xval:zval],16)    # S1 length?
#   
          dfout = []
          dfout.append(sref)
          dfout.append(dcval)   
          mlist.append(dfout) 
#      
       s1len = mlist[4][1] 
       if s1len == 22 :
          stp = 60          
       elif s1len == 23:
          stp = 62 
       else:          
          sxp = errd['5'][1]    
    #      sld = sf + '-' + str(xd) 
          sld = sf + '-' + str(xd) + '-' + sfd 
          sxp.append(sld)
          errd['5'][1] = sxp     
          return(mdict,errd)
#
       s2ind = mlist[9][1]
#  
       if s2ind > 0:
            
          s2len = int(tlist[stp:stp+6],16)
          s2lenf = s2len * 2        
       else:
          s2lenf = 0   
#
       s3stp = stp + s2lenf
       s3len = tlist[s3stp: s3stp+6]    
       s3lend = int( s3len ,16)
       s3lenf = s3lend * 2    
       dfout = []
       dfout.append('Section 3 length ')
       dfout.append(s3lenf)          
       mlist.append(dfout)         
# 
       ds3len = s3lenf
       s3data = tlist[s3stp + 14 : s3stp + ds3len]    
       s3i = hexread(s3data)
       dfout = []     
       dfout.append('Section 3 data ' )
       dfout.append(s3i)                      
       mlist.append(dfout)       
       keym = mfile + '-' + str(tnob)
       mdict[keym]= mlist               
    return(mdict,errd) 
#
#############################################################################
#
#
# subroutine : 	   hexread
#
# purpose:         convert hex string to FXY values 
#                            1. Read hex string and divide into
#                               4 character hex substrings
#                            2. Convert the 4 character HEX string into 
#                               16 bit binary string and sub divide in 
#                               a 2 : 6 : 8 ratio.                                                      
#                            3. Convert the binary substrings to decimal and 
#                               recombine to form the FXY Descriptor
#
# parameters:
#                   input =>
#                   srex : HEX String
 
#                   output
#                   String of FXY Descriptors
#
#
#############################################################################
#
def hexread(srex):

   n = 0
   tsd1 = []
   for xp in range(0,len(srex),4):
     n1 = xp
     n2 = xp + 4
     if n2 <= len(srex):
        tvla = srex[n1:n2]
        tsd1.append(tvla)
#
   tnval3 = []
   for xpz in range(0,len(tsd1)):
      tval = bin(int(tsd1[xpz], 16))[2:].zfill(16)
      tnval3.append(tval)
#
   tdv = []
   for dv in range(0,len(tnval3)):
      tval = tnval3[dv]
      tv1 = tval[0:2]
      tv2 = tval[2:8]
      tv3 = tval[8:] 
      td1 = str(int(tv1,2))
      td2 = str(int(tv2,2)).zfill(2)
      td3 = str(int(tv3,2)).zfill(3)
      tdm = td1 + td2 + td3
      tdv.append(tdm)
 # 
   return(tdv)
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
#                  tfref :  Date / Time list of the format   
#                           [datatype, data_directory, nedted list containing 
#                           start date (Day / Month / Year : 'Time')                                                                      
#
#############################################################################
#
def listing_build(ah,tsp1):
#
    ah2=ah[5]                                   # extract dir path for the files 
    dtp = ah[0]  
    now = dt.datetime.now()
    ago = now-dt.timedelta(minutes=tsp1)
    flist = []
    tmref = []
    tfref = []
    for root, dirs, files in os.walk(ah2):      # generate list of filenames
                                                # from the processed directory.
        for fname in files:
            path = os.path.join(root, fname)
            st = os.stat(path)
            mtime = dt.datetime.fromtimestamp(st.st_mtime)  
            if mtime > ago:
                tmref.append(mtime)      
                flist.append(path)
    mxref = max(tmref)
    mnref = min(tmref)
    tfref.append(dtp)
    tfref.append(mnref.strftime("%d/%m/%Y, %H:%M:%S"))
    tfref.append(mxref.strftime("%d/%m/%Y, %H:%M:%S"))
    return(flist,tfref)
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
#                  errpq    :  Dictionary of encoding / decoding errors
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
def fprocess(indct,errq):
#
    brdict = {}
    brdict2 = {}
    brdict3 = {}
    nm = {}
    nrrf = -1
#         
    for mik in indct.keys():
        dty = indct[mik][0][0]          # data type  'SONDE Etc'         
        dataf = indct[mik][0][8]        # last entry in the list will be a        
                                        # listing of the MHSR data associated
                                        # with the header ranges etc.
        hddata = indct[mik][0][7]       # header ranges in hex
        hrange = indct[mik][0][6]       # element index sequences
#
        if len(dataf) == 0:
            break
        else:
            for filein in dataf:
                if len((filein.strip())) == 0:
                    break                           
                else: 
                    (brdict,errw) = sum_bin(filein,errq,dty)                                              
                    for keyx in brdict.keys():                                                                                                                  
                       abx = brdict[keyx]            
                       (abf) = thin_ob(abx,dty)                         
                       if abf == 1:                                       
                          abx.append(dty)
                          abx.append(hddata)
                          abx.append(hrange) 
                          if keyx not in nm.keys():
                             nm[keyx] = abx                                                                                               
#                       
    return(nm,errw)
#
#############################################################################
#
#
# subroutine name: read_config
#
# purpose:         read config file. Output details to a dict of lists.
#
# input:           cnfgin     :      config file name
#                  trigger    :      machine identification supplied
#                                    by os.environ['MACHINE']                          
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
#
#############################################################################
#
# subroutine name: thin_ob
#
# Purpose : check the individual observation TTAAii CCCC against the 
#           definition supplied in Volume C1. The list output only includes
#           data whose TTAAii conforms to that supplied by Vol C1 for a 
#           specific data type  
#
#  
#
#############################################################################
#
def thin_ob(obin,dtp):
#
   dataid = {}
   dataid['BUOYB'] = [['IOB','01-25','BUOY']]
   dataid['ARGOB'] = [['IOP','01-59','TESAC']]
   dataid['SYNOPT'] = [['ISA','01-59','SYNOP'],['ISI','01-59','SYNOP'],['ISM','01-59','SYNOP'],\
                     ['ISN','01-59','SYNOP']]
   dataid['CLIMAT'] = [['ISC','01-59','CLIMAT']]
   dataid['SHIPB'] = [['ISS','01-59','SHIP']]
   dataid['AMDAR'] = [['IUA',' ','AMDAR']]
   dataid['PILOT'] = [['IUJ','01-59','PILOT'],['IUW','01-59','PILOT']]
   dataid['SONDE'] = [['IUK','01-59','TEMP']]
   dataid['SONDE'] = [['IUS','01-59','TEMP']]
#     
# compile header ranges from data type(dtp)
#
   drange = dataid[dtp]   
# 
   drm = []
   for xt in range(0,len(drange)):
      drf = []
      vx1 = drange[xt][0]
      vsp = drange[xt][1].split('-')    
      vx3 = vx1 + vsp[0]
      vx4 = vx1 + vsp[1]
      hrng1=binascii.b2a_hex(vx3)
      brng1=bin(int(hrng1,16))[2:]
      inrng1=int(brng1,2)
#
      hrng2=binascii.b2a_hex(vx4)
      brng2=bin(int(hrng2,16))[2:]
      inrng2=int(brng2,2)
#
      drf.append(inrng1)
      drf.append(inrng2)
      drm.append(drf)
#
   cdf = obin[0][0]
   cdp1 = cdf[0:4][0:3]
   cdp2 = cdf[0:4][4:6]
   cdf3 = cdp1 + cdp2
   hdg2=binascii.b2a_hex(cdf3)
   hbg2=bin(int(hrng2,16))[2:]
   hbfg2=int(brng2,2)
#
   passx = -1
   for xz in range(0,len(drm)):
      df1 = drm[xz][0]
      df2 = drm[xz][1]    
      if hbfg2 >= df1 and hbfg2 <= df2:
         passx = 1
         break
#
   return(passx)
#
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
#  final processing
# 
#  name: fnproc
#
#  Purpose : recondition data prior to output.   

#
#############################################################################
#
def fnproc(dictin,errw):
#  
   nx = 0
   nz = 0
   for keyx in dictin.keys():
      dtp =  dictin[keyx][23] 
      hrange = dictin[keyx][24]                # header range
      headid = dictin[keyx][0][0].split(' ')   # TTAAii CCCC
      headidr = headid[0]                      # TTAAii                                                      
      hdval = binascii.b2a_hex(headid[0])      # ['TTAAii']
      hdval2=bin(int(hdval,16))[2:]
      hdval3=int(hdval2,2)
      hdval = hdval3
      encseq = dictin[keyx][22][1]             # FXY Encoding sequence (section 3)                  
      indseq = dictin[keyx][25]                # Element Index Sequences                  
#
      tval = -1
      for xp in range(0,len(hrange)):
         trng0 = hrange[xp][0]
         trng1 = hrange[xp][1]
         if hdval >= trng0 and hdval <= trng1 :
            tval = 1
            break             
      if tval == -1:
         nx +=1
         errw['6'][1] =  dictin[keyx][21:]     # TTAAii outside of header range
      else:
         nz += 1          
      xval = -1
      for xd in range(0,len(indseq)):            
         if set(encseq) <= set(indseq[xd]) :  # Encoding sequence not represented 
                                              # in the Elements Index ??       
            xval = 1        
            break
      if xval == -1:         
         ad_val1 = keyx + '-' + dtp
         def1 = []
         def1.append(ad_val1) 
         def1.append(dictin[keyx][0][0]) 
         def1.append(encseq)      
         def2 = errw['1'][1]
         def2.append(def1)       
         errw['1'][1] = def2  
#        
   return(errw)                  
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
cfin='/home/moodsf/D_sequence_comparison/config_table.txt'
#
osin = os.environ['MACHINE']
wout = os.environ['WEB_HTML_DIR']
cnfg=read_config(cfin,osin)  
#
errqd = {}
errqd['1'] = [['Encoding sequence not represented in the Elements Index'],[]]
errqd['2'] = [['BUFR or 7777 missing from Bulletin'],[]]
errqd['3'] = [['Incorrect value for BUFR Edition'],[]]
errqd['4'] = [['TTAAii not identified'],[]]
errqd['5'] = [['Incorrect length for Section 1'],[]]
errqd['6'] = [['TTAAii outside of header range'],[]]
#  
elist=[]                                # Temporary list (position of elements
                                        # index                                   
brdictxf={}                             # Final out put dictionary, will 
                                        # contain processed data ready to be 
                                        # output by web_dout        
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
#############################################################################
#
xp=1440                                 # constant (in mins) used to 
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
# 
dpq = {}
#
for cn1 in cnfg.keys():
    dlist = []
    dlist.append( cnfg[cn1][0][0])
    dlist.append( cnfg[cn1][0][5])                                          
    df = cnfg[cn1]
    ldf = len(df)
    for yx in range(0, ldf):                                      
                                        # compile listing of data in dir 
                                        # and header file in HEX     
        hdn=df[yx][1]
        din=df[yx]    
        sn = df[yx][0]
        (slist,tlist) = listing_build(din,xp)   # take listing of the MHSR files 
                                                # fitting the date / time criteria
        plist = read_hdr(hdn,sn)                # read the header file   
        df[yx].append(plist)
        df[yx].append(slist)
#    
    dlist.append(tlist[1:]) 
    dpq[cn1] = dlist
    cnfg[cn1]=df                                     
#          
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
(brdictxf,erryx) = fprocess(cnfg,errqd) 
#
erryx2 = fnproc(brdictxf,erryx)  
# 
dt2 = '/home/moodsf/D_sequence_comparison/rogue_d_seq.txt'
#
i=datetime.now() 
xt=i.strftime('%Y/%m/%d %H:%M:%S') 
#
#  construct and output to a *.csv file_in#
#  
with open(dt2, 'w') as fmx:   
   for keyx in erryx2.keys():
      if keyx == '1':
         ft1='{message:{fill}{align}{width}}'.format(message = keyx ,fill=' ',align='<',width=5)
         ft2='{message:{fill}{align}{width}}'.format(message = erryx2[keyx][0][0] ,fill=' ',align='<',width=10)     # error title            
         for xv in range(0,len(erryx2[keyx][1])):     
            ft31 =  erryx2[keyx][1][xv][0].split('-')
            ft31a = ft31[0]
            ft31b = ft31[1] 
            ft31c = ft31[2]  
            lft3a = len(ft31a)
            lft3b = len(ft31b) 
            lft3c = len(ft31c)                
            ft3 = '{message:{fill}{align}{width}}'.format(message = ft31a ,fill=' ',align='<',width=lft3a)     # mhsr.....
            ft4 = '{message:{fill}{align}{width}}'.format(message = ft31b ,fill=' ',align='<',width=lft3b)     # ob no
            ft4c = '{message:{fill}{align}{width}}'.format(message = ft31c ,fill=' ',align='<',width=lft3c)     # data type
            ft5 = '{message:{fill}{align}{width}}'.format(message = erryx2[keyx][1][xv][1] ,fill=' ',align='<',width=11)     # TTAAii CCCC
            ojlist = ' '.join(erryx2[keyx][1][xv][2])
            ljlist = len(ojlist)    
            ft6 = '{message:{fill}{align}{width}}'.format(message = ojlist ,fill=' ',align='<',width=ljlist)
            fmx.write(('%s')%(ft1)) 
            fmx.write(('%s')%(' , '))        
            fmx.write(('%s')%(ft2)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s')%(ft3))   
            fmx.write(('%s')%(' , '))    
            fmx.write(('%s')%(ft4))    
            fmx.write(('%s')%(' , '))
            fmx.write(('%s')%(ft4c))    
            fmx.write(('%s')%(' , '))
            fmx.write(('%s')%(ft5))  
            fmx.write(('%s')%(' , '))    
            fmx.write(('%s\n')%(ft6))   
      elif keyx == '2' :
         fx1='{message:{fill}{align}{width}}'.format(message = keyx ,fill=' ',align='<',width=5)
         fx2='{message:{fill}{align}{width}}'.format(message = erryx2[keyx][0][0] ,fill=' ',align='<',width=10)     # error title    
         for xz in range(0,len(erryx2[keyx][1])):
            fxz1 =  erryx2[keyx][1][xz].split('-')
            fname = fxz1[0]
            fdtype = fxz1[1]
            fx3a = '{message:{fill}{align}{width}}'.format(message = fname,fill=' ',align='<',width=10)     # MHSR filename 
            fx3b = '{message:{fill}{align}{width}}'.format(message = fdtype,fill=' ',align='<',width=10)     # Data type 
            fmx.write(('%s')%(fx1)) 
            fmx.write(('%s')%(' , '))        
            fmx.write(('%s')%(fx2)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s')%(fx3a)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s\n')%(fx3b))     
      elif keyx == '3' :
         fx1='{message:{fill}{align}{width}}'.format(message = keyx ,fill=' ',align='<',width=5)
         fx2='{message:{fill}{align}{width}}'.format(message = erryx2[keyx][0][0] ,fill=' ',align='<',width=10)     # error title    
         for xz in range(0,len(erryx2[keyx][1])):
            fx3 = '{message:{fill}{align}{width}}'.format(message = erryx2[keyx][1][xz],fill=' ',align='<',width=10)     # error entries 
            fmx.write(('%s')%(fx1)) 
            fmx.write(('%s')%(' , '))        
            fmx.write(('%s')%(fx2)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s\n')%(fx3))
      elif keyx == '4' :   
         fx41='{message:{fill}{align}{width}}'.format(message = keyx ,fill=' ',align='<',width=5)
         fx42='{message:{fill}{align}{width}}'.format(message = erryx2[keyx][0][0] ,fill=' ',align='<',width=10)     # error title 
         for xv in range(0,len(erryx2[keyx][1])):
            ft41 =  erryx2[keyx][1][xv].split('-')             
            ft41a = ft41[0]
            ft41b = ft41[1]  
            ft41c = ft41[2]      
            lft4a = len(ft41a)
            lft4b = len(ft41b)    
            lft4c = len(ft41c) 
            fx43 = '{message:{fill}{align}{width}}'.format(message = ft41a ,fill=' ',align='<',width=lft4a)     # mhsr.....
            fx44 = '{message:{fill}{align}{width}}'.format(message = ft41b ,fill=' ',align='<',width=lft4b)     # ob no
            fx44 = '{message:{fill}{align}{width}}'.format(message = ft41c ,fill=' ',align='<',width=lft4c)     # ob no
            fmx.write(('%s')%(fx41)) 
            fmx.write(('%s')%(' , '))        
            fmx.write(('%s')%(fx42)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s')%(fx43))
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s\n')%(fx44))       
      elif keyx == '5' : 
         fx1='{message:{fill}{align}{width}}'.format(message = keyx ,fill=' ',align='<',width=5)
         fx2='{message:{fill}{align}{width}}'.format(message = erryx2[keyx][0][0] ,fill=' ',align='<',width=10)     # error title 
         for xv in range(0,len(erryx2[keyx][1])):
            ft51 =  erryx2[keyx][1][xv].split('-')
            ft51a = ft51[0]
            ft51b = ft51[1]  
            ft51c = ft51[2]
            lft5a = len(ft51a)
            lft5b = len(ft51b)
            lft5c = len(ft51c)                   
            fx53 = '{message:{fill}{align}{width}}'.format(message = ft51a ,fill=' ',align='<',width=lft5a)     # mhsr.....
            fx54 = '{message:{fill}{align}{width}}'.format(message = ft51b ,fill=' ',align='<',width=lft5b)     # ob no
            fx55 = '{message:{fill}{align}{width}}'.format(message = ft51c ,fill=' ',align='<',width=lft5c)     # ob no
            fmx.write(('%s')%(fx1)) 
            fmx.write(('%s')%(' , '))        
            fmx.write(('%s')%(fx2)) 
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s')%(fx53))
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s')%(fx54))
            fmx.write(('%s')%(' , ')) 
            fmx.write(('%s\n')%(fx55))
  
   for keyz in dpq.keys():   
         
      fz0a = 'Data Type,' + dpq[keyz][0] + ','
      fz1a = 'Data Directory,' + dpq[keyz][1]+ ','  
      tmp = dpq[keyz][2][0] + ',' + dpq[keyz][2][1] 
      fz0 = '{message:{fill}{align}{width}}'.format(message = fz0a ,fill=' ',align='<',width=len(fz0a))
      fz1 = '{message:{fill}{align}{width}}'.format(message = fz1a ,fill=' ',align='<',width=len(fz1a))  
      tmpz = '{message:{fill}{align}{width}}'.format(message = tmp ,fill=' ',align='<',width=len(tmp))    
      fmx.write(('%s')%(fz0))
      fmx.write(('%s')%(fz1))
      fmx.write(('%s\n')%(tmpz))
#
   ft1 = 'Run Time - ' + xt
   ft1a = '{message:{fill}{align}{width}}'.format(message = ft1 ,fill=' ',align='<',width=len(ft1))
   fmx.write(('%s\n')%(ft1a))
   fmx.close()   
#
#############################################################################
#