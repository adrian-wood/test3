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
##############################################################################################################################################################################################################
#
# Name:             data_feed_comparison.py
# 
# purpose:          Testing of the D sequence used to encode MHSR bulletins to ensure that a mapping exists with the current element indexes.
# 
# subroutines:      read_c11  =>   Read the txt version of common code table C11 into dict of lists
#                   parameters: 
#                   input => 
#                            file_in - Filename of txt version of common code table C11   	
#  
#                   output
#                   c11_read - dict of lists. 
#
#                   read_c12  =>   Read the txt version of common code table C12 into dict of lists
#                   parameters: 
#                   input => 
#                            file_in - Filename of txt version of common code table C12   	
#  
#                   output
#                   c12_read - dict of lists.                       
#                   
##############################################################################################################################################################################################################
#
# subroutine : 	    ind_read
# 
# purpose:          Read the retrieval elements index (new type)
# 
# parameters: 
#                   input =>    	
#	            S1    -   full filename of the retrieval index file.
# 	           
#                   output => 
#                   flist2 - list containing D sequences from the index 
#                            file sections 1 - 3 of the MHSR file.   
#
##############################################################################################################################################################################################################
#
def ind_read(S1):
 tlist=[]
 n=-1
 B1='BUFR SEQUENCES'
 I1='INDEX' 
 lenS1=len(S1)-1
 n=0
 slist=[]
 flist=[]
 plist=[]
 start = 0
 with open(S1,'r') as fi:                                                  # Open the element index as Read only
    iname=S1.split("/")[-1:][0]
    for line in fi:      
       line=line.replace("\n","") 
       if line.find(B1) > -1:                                              # The D sequence values in the retrieval index are bounded by             
          start=1                                                          # the strings  'BUFR SEQUENCES & 'INDEX'   
          line=fi.next() 
       elif line.find(I1) > -1 or len(line.replace(" ","")) == 0:          # end of index values  
           flist.append(plist)
           break 
       if start == 1:
          tlist=line[0:11].replace(" ","")                                 # test for new sequence                            
          slist=(line[12:].split(' '))                      
          if len(tlist) > 0:                                               # start of new index sequence  
             if len(plist) != 0:
                 if n > 0:     
                   flist.append(plist)
                   plist=[]
                 plist=slist  
             else:
                 plist=slist      
          else:      
             plist=plist+slist 
       n = n+1 
    flist2=flist[1:]   
 fi.close()
 return(flist2)
##############################################################################################################################################################################################################
#
# subroutine : 	    sum_bin
# 
# purpose:          Will read (as bin) sections 1 - 3 of the MHSR file. 
# 
# parameters: 
#                   input =>    
#                   hf      -  header file
#                   dtp	    -  data type
#	            S1      -  full filename of the MHSR file.
# 	            c11_d   -  filename common code table C11
#                   c12_d   -  filename common code table C12
#                   errq    -  Error dictionary 
#
#                   output => sum_dict - dict containing Decoded (bin - ascii) data 
#                                        from sections 1 - 3 of the MHSR file.   
#                             errq     - dictionary containing error messages
#                             uttc     - flag (set to either 1 or -1) to indicate the presence of an 
#                                        encoding D sequence 
#
#
##############################################################################################################################################################################################################
#
def sum_bin(hf,dtp,S1,c11_d,c12_d,errq):
 with open(S1,'rb') as fp:                                                        # open bufr and read as binary  
    fdict={} 
    sum_dict={}
    bst='xxxx'
    td=['X','X']
    obn=1
#   hdef = Hex equivalents of - ASCII
#             'I' 'E'     'I'  'N'     I   'O'   'I '  'P'   'I' ,'S'     'I'  'T'    'I','U'  
    hdef = [['49','45'],['49','4e'],['49','4f'],['49','50'],['49','53'],['49','54'],['49','55']]
    S1a=S1.split("/")
    S1f=S1a[-1:][0]  
    data=fp.read(1)                                                          
    btnum=-1
    sbt=0
    ebt=0
    flistbta=[]
    flistbth=[] 
    btn=-1
    sbt=0
    ebt=0
    uttc = -1
    list_temp=[]
    stp = -1
    while data != "":                                                         # read through  data 1 byte at a time
      data=fp.read(1)                                                         # data read in will be binary     
      btn=btn+1                                                                    
                                                                              # and stored into list within a dict as HEX.
                                                                              # In order to recognise the start and end
                                                                              # of the message a conversion to ascii will be needed.
      fstring1=binascii.b2a_hex(data)                                         # convert binary to hex
      fstringb=fstring1.decode('hex')
      fstringt=binascii.b2a_qp(data,quotetabs=0,istext=0,header=0)            # convert binary to ascii 
      bst1=bst[0:3]+fstringb                                                  # rotate 4 character array until the end of bull is reached
      bst=bst1[1:4]
      if bst1 != "7777" :
         list_temp.append(fstring1)                                           # append converted byte (now in HEX) to a temp list
         if bst1 == 'BUFR':
           stp = btn -3
      else:
         if stp > -1:
            list_temp.append(fstring1)                                        # append converted byte (now in HEX) to a temp list
            bsetc=str(obn) 
            ob_fno=int(bsetc)
            ept = btn -3                                                      # each "message" will be written to a dict with the ob_no as the key
            stend=[]
            stend.append(stp)                                                 # start point 'BUFR ' found    
            stend.append(ept)
            list_temp.append(stend)                                           # end point '7777 ' found   
            kob=S1+'-'+str(bsetc)  
            fdict[kob]=list_temp   
            list_temp=[]   
            obn=obn+1                                                         # at end of message reset the list
            btn = -1 
         else:
            bro1 = errq['Unable to find BUFR in message']
            key2=S1.split('/')
            key3=key2[-1:][0].split('-')
            keyf=key3[0]
            bro2=[]
            bro2.append(keyf) 
            bro2.append('-')
            bro2.append('-')
            bro1.append(bro2)
            errq['Unable to find BUFR in message'] = bro1   
            uttc = -1
            sum_dict=[]
            return(sum_dict,errq,uttc)
 fp.close()
#
 for key in fdict.keys():                                                     # fdict will contain [['42','55','46','52'...'37','37','37','37],[st_point,end_point]]    
      listf=fdict[key]                                                        # for each bulletin   will be of the form 
                                                                              # /var/moods/bulletins/processed//SNT1/MHSR.R2D17334.T091905.SNT1.s803-3     
      stpnm=[] 
      for xz in range(0,(len(listf))):                                        # rotate through list check for match against hdef ('I E','I N' etc) in hex   
          td.append(listf[xz])
          td.pop(0)
          if td in hdef:
             stpn=xz-1
             stpnm=listf[stpn:stpn+11]
             ttaa=[]                                                          # if match found mark the position of the matching octet add to list stpnm  
             break    
      if len(stpnm) == 0: 
         break                                                                # if no match found (ie list length = 0) break
      for xy in range(0,len(stpnm)):
          vla=stpnm[xy].decode('hex')
          ttaa.append(vla)
          ttaaf=''.join(ttaa[:])                                              # translate from HEX to ascii for TTAAii CCCC       
      stpnt=int(listf[-1:][0][0])                                             # start and end points will be in nested list at the end of the data   
      tlist=listf[stpnt:]
      if len(tlist) == 0:                                                     # temp workaround to allow for len(tlist) == 0 
        break
      tlen = int(''.join(tlist[4:7]),16)                                      # join ocets 4:7 and convert hex => int. Overall Bull length.
      hdl=len(tlist)                                                  
      idlfj=tlist[7:8]                                                        # join ocets 7:8 and convert hex => int. BUFR Edition.
      bedi=int(idlfj[0],16)
      if int(bedi) == 4:                                                      # assume and read as read BUFR edition 4
         s1len = int(''.join(tlist[8:11]),16)                                 # section 1 length                                             # If length of section 1 is 22 add extra octet to make len 23  
         bmt = int(tlist[11:12][0],16)                                        # ocets 11:12 convert hex => int. BUFR Master Table. 
         idgc = int(''.join(tlist[12:14]),16)                                 # ocets 12:14 convert hex => int. ID Generating centre.      
         idgc2=str(idgc)             
         if idgc2 in c11_d.keys():
            idgcf=c11_d[idgc2]
            idgc=idgcf                                                        # cross reference against Table C11 and add entry to output      
         else:
            idgcf="N/A" 
            idgc=idgcf    
         idgsc = int(''.join(tlist[14:16][0]),16)                             # ocets 14:16 convert hex => int. ID Generating sub centre.    
         idgsc2=str(idgsc)             
         if idgsc2 in c12_d.keys():
            idgscf=c11_d[idgsc2]
            idgsc=idgscf  
         else:
            idgscf="N/A" 
            idgsc=idgscf  
#  
         try: 
             yrd = int(''.join(tlist[23:25]),16)                              # octets 23:25 (Edition 4) YEAR
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring Year']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring Year']=ero
             break
#
         try: 
             mrd = int(''.join(tlist[25:26][0]),16)                           # octets 25:26 (Edition 4) Month
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]   
             ero = errw['Binary Read error in aquiring Month']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)  
             erot.append(ttaaf) 
             ero.append(erot)   
             errw['Binary Read error in aquiring Month']=ero
             break
#
         try: 
             drd = int(''.join(tlist[26:27][0]),16)                           # octets 26:27 (Edition 4) Day
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]  
             ero = errw['Binary Read error in aquiring day']
             erot=[]
             erot.append(keyf) 
             erot.append(obn) 
             erot.append(ttaaf)     
             ero.append(erot)   
             errw['Binary Read error in aquiring day']=key
             break
#
         try:
             hrd = int(''.join(tlist[27:28][0]),16)                           # octets 27:28 (Edition 4) hour         
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]  
             ero = errw['Binary Read error in aquiring hour']
             erot=[]
             erot.append(keyf) 
             erot.append(obn) 
             erot.append(ttaaf)   
             ero.append(erot)   
             errw['Binary Read error in aquiring hour']=key 
             break 
#
         try:
             mnrd = int(''.join(tlist[28:29][0]),16)                          # octets 228:29 (Edition 4) Minute
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]  
             ero = errw['Binary Read error in aquiring minute']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf) 
             ero.append(erot)  
             errw['Binary Read error in aquiring minute']=key  
#
         try:
             msec = int(''.join(tlist[29:30][0]),16)                          # octets 29:30 (Edition 4) second
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]  
             ero = errw['Binary Read error in aquiring second']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)   
             errw['Binary Read error in aquiring second']=key  
             break
#
         s2indp = tlist[17:18][0]                                             # Octets 17:18 indicate the presence or lack of section 2
         s2ind=bin(int(s2indp, 16))[2:].zfill(8)                              # convert HEX to binary string    
                                                                              # Bit1 of the binary string indictaes the inclusion or exclusion of section 2               
         if s2ind[0] == '0':                                                  # Bit 1 = 0 indicates No optional section 2   
            s2len = 0   
            s3stpt = 8 + s1len + s2len                     
            s3len = int(''.join(tlist[s3stpt:s3stpt+3]),16)                   # assuming no section 2 octets 31:34 will give the length of section 3 
            s3lhex = tlist[s3stpt:s3stpt+3]
             
            if s3len >=  tlen or s3len == 0:                                  # id len section 3 == 0 capture error   
                
                ero = errq['Incorrectly encoded value for section 3 length (section 2 not present) - BUFR Edition 4']
                key2=key.split('/')
                key3=key2[-1:][0].split('-')
                keyf=key3[0]
                obn=key3[1]  
                erot=[]
                erot.append(keyf) 
                erot.append(obn)
                erot.append(ttaaf)
                ero.append(erot)
                errq['Incorrectly encoded value for section 3 length (section 2 not present) - BUFR Edition 4'] = ero
                break   
            s3ds =  tlist[s3stpt+7:s3stpt + s3len]                            # bulletin encoding sequence
         else:                                                                # assuming section 2 present 
        #    s2len = int(''.join(tlist[32:34]),16)                             # octets 31:34 will give the length of section 2 
            s2stpt = 8 + s1len
            s2len = int(''.join(tlist[s2stpt:s2stpt+3]),16)        
            s3stpt = 8 + s1len + s2len     
            s3len = int(''.join(tlist[s3stpt:s3stpt+3]),16)                   # section 3 length  
            if s3len >=  tlen or s3len == 0 :
                ero = errq['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 4'] 
                key2=key.split('/')
                key3=key2[-1:][0].split('-')
                keyf=key3[0]
                obn=key3[1]  
                erot=[]
                erot.append(keyf) 
                erot.append(obn)
                erot.append(ttaaf)
                ero.append(erot) 
                errq['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 4'] = ero
                break   
            s3ds =  tlist[s3stpt+7:s3stpt + s3len]                            # bulletin encoding sequence 
      elif int(bedi) == 3:                                                    # read as BUFR edition 3
         s1len = int(''.join(tlist[8:11]),16)                                 # read and convert ocets 8:11 => for section 1 length    
         if s1len == 17:
            tlist.insert(31,'00')                                             # add extra octet to make section 1 length to 18
         bmt = int(tlist[11:12][0],16)                                         
         idgc = int(''.join(tlist[12:13][0]),16)                              # ID of the generating centre
         idgsc = int(''.join(tlist[13:14]),16)                                # ID of the sub generating centre
         idgc2=str(idgc)   
         idgsc2=str(idgsc)           
         if idgc2 in c11_d.keys():
            idgcf=c11_d[idgc2]
            idgc=idgcf  
         else:
            idgcf="N/A" 
            idgc=idgcf                 
         if idgsc2 in c12_d.keys():
            idgscf=c11_d[idgsc2]
            idgsc=idgscf  
         else:
            idgscf="N/A" 
            idgsc=idgscf   
# 
         try: 
             yrd = int(''.join(tlist[20:21]),16)                              # Year
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring Year']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring Year']=ero           
             break
#
         try: 
             mrd = int(''.join(tlist[21:22][0]),16)                           # Month    
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring Month']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring Month']=ero     
             break
#
         try: 
             drd = int(''.join(tlist[22:23][0]),16)                           # Day
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring day']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring day']=ero     
             break
#         
         try:
             hrd = int(''.join(tlist[23:24][0]),16)                           # Hour
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring hour']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring hour']=ero     
             break 
#
         try:
             mnrd = int(''.join(tlist[24:25][0]),16)                          # Minute
         except:
             key2=key.split('/')
             key3=key2[-1:][0].split('-')
             keyf=key3[0]
             obn=key3[1]                 
             ero = errw['Binary Read error in aquiring minute']
             erot=[]
             erot.append(keyf) 
             erot.append(obn)
             erot.append(ttaaf)
             ero.append(erot)      
             errw['Binary Read error in aquiring minute']=ero    
# 
         msec = '0'   
         s2indp = tlist[15:16][0]                                             # section 2 indicator
         s2ind=bin(int(s2indp, 16))[2:].zfill(8)   
         if s2ind[0] == '0':                                                  # section 2 indicator (bit1 == 0)  
            s3len = int(''.join(tlist[31:34]),16)  
            if s3len >=  tlen :
                ero = errq['Incorrectly encoded value for section 3 length (section 2 not present) -  BUFR Edition 3']
                key2=key.split('/')
                key3=key2[-1:][0].split('-')
                keyf=key3[0]
                obn=key3[1]  
                erot=[]
                erot.append(keyf) 
                erot.append(obn)
                erot.append(ttaaf)
                ero.append(erot)  
                errq['Incorrectly encoded value for section 3 length (section 2 not present) -  BUFR Edition 3'] = ero
                break   
            s3ds =  tlist[33:26+s3len]  
         else: 
            s2len = int(''.join(tlist[26:29]),16) 
            s3len = int(''.join(tlist[(8 + s1len + s2len):(8 + s1len + s2len + 3)]),16)           # Section 3 length   
            if s3len >=  tlen :
                ero = errq['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 3']
                key2=key.split('/')
                key3=key2[-1:][0].split('-')
                keyf=key3[0]
                obn=key3[1]  
                erot=[]
                erot.append(keyf) 
                erot.append(obn)
                erot.append(ttaaf)
                ero.append(erot)  
                errq['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 3'] = ero
                break   
            s3ds =  tlist[39:39+s3len] 
      elif int(bedi) != 3 and int(bedi) != 4 :                                # error trap, unable to identify BUFR edition  
          ero =  errq['Unable to identify BUFR Edition'] 
          key2=key.split('/')
          key3=key2[-1:][0].split('-')
          keyf=key3[0]
          obn=key3[1]  
          erot=[]
          erot.append(keyf) 
          erot.append(obn)
          erot.append(ttaaf)
          ero.append(erot) 
          errq['Unable to identify BUFR Edition'] = ero   
          break     
      if len(s3ds) != 0:           
         s3pat=[0,2,6]
         fxy=1  
         s3ind=bit_anal(s3ds,s3pat,fxy)                                       # convert the encoding sequence from hex to ascii 
         uttc = 1  
      else:
         uttc = -1  
#          
      list_temp=[]
      list_temp.append(ttaaf)
      list_temp.append(bedi)
      list_temp.append(bmt) 
      list_temp.append(idgc)
      list_temp.append(idgsc)
      list_temp.append(yrd)
      list_temp.append(mrd)
      list_temp.append(drd)
      list_temp.append(hrd)
      list_temp.append(mnrd)
      list_temp.append(msec)
      list_temp.append(s3ind)
      if key not in sum_dict.keys():
           sum_dict[key]=list_temp
 return(sum_dict,errq,uttc) 
#
##############################################################################################################################################################################################################
#
# subroutine : 	    bit_anal
# 
# purpose:          Convert single or multiple HEX value(s) to Binary. Split the binary string as outlined in bit pattern (bitp) and convert the 
#                   resultant BIN values to integers
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
##############################################################################################################################################################################################################
#
def bit_anal(spval,bitp,fxy):
#   sp=0 
   numb=8
   scale=16 
   olist=[]
   flist=[]
   lenbp=len(bitp)
   lensp=len(spval)
   for x in range(0,lensp):
      cnv=bin(int(spval[x],scale))[2:].zfill(numb)                                # convert the whole of the HEX value to a binary string 
      olist.append(cnv)                                                           #     
   lenbsp=len(olist)       
   if fxy :                                                                       # if fxy selected (=TRUE) then 2 bytes will be needed
      for y in range(0,lenbsp):
         yx=2*y 
         if yx < (lenbsp-1):
           val1=olist[yx]
           val2=olist[yx+1]
           valf=val1+val2
           fval=str(int((valf[0:2]),2)) 
           xval=int((valf[2:8]),2)
           if xval < 10:
             xvals="0"+(str(xval))
           else:
             xvals=str(xval) 
           yval=int((valf[8:]),2) 
           if yval < 100:  
             if yval < 10:
               yvals="00"+(str(yval))
             else:
               yvals="0"+(str(yval))
           else:
             yvals=str(yval)    
           fxyval=fval+xvals+yvals
           flist.append(fxyval) 
   else:                                                                          # assumes a single binary string
      for y in range(0,lenbp):
         if y < (lenbp-1):
            valf=olist[0] 
            rn1=bitp[y]
            rn2=bitp[y+1]
            val=str(int((valf[rn1:rn2]),2))
            flist.append(val)
         else:
            rn1=bitp[y]  
            val=str(int((valf[rn1:]),2))        
            flist.append(val) 

 # 
   return(flist)
#   
##############################################################################################################################################################################################################  
#
#
# subroutine : 	    read_c11
# 
# purpose:         Read in common code table C11 to a dict of lists 
#
# parameters: 
#                   input => 
#                   file_in - Filename common code table C11   	
#  
#                   output
#                   c11_read - dict output  
#
############################################################################################################################################################################################################## 
#
def read_c11(file_in):
  c11_read={}
  rg="Region"
  with open (file_in,'r') as fb: 
    fb.next()
    fb.next()
    fb.next()          
    for line in fb:
      stringtf=line.replace(";"," ")                     
      stringtf=stringtf.replace("\",\"",";")
      stringtf=stringtf.replace("\",,\"",";;")
      stringtf=stringtf.replace("\",,,\"",";;;")
      stringtf=stringtf.replace(",\"",";")
      stringtf=stringtf.replace("\",",";")
      stringtf=stringtf.replace("\"","")
      stringtf=stringtf.replace(","," ")
      stringt=stringtf.split(";")
      test_st=stringt[1]
      test_l=len(stringt)
      tno=test_st.find("rg")
      if tno == -1 and test_l > 4:
         key=stringt[2]
         ent=stringt[3]
         c11_read[key]=ent 
  return(c11_read)
###############################################################################################################################################################################################################    
#
#
# subroutine : 	    read_c12
# 
# purpose:         Read in common code table C12 to a dict of lists 
#
# parameters: 
#                   input => 
#                   file_in2 - Filename common code table C12   	
#  
#                   output
#                   c11_read - dict output  
#
##############################################################################################################################################################################################################
def read_c12(file_in2):
   c12_read={}
   templ=[]
   ml=[]
   rg="Region"
   with open (file_in2,'r') as fb:  
     fb.next()
     fb.next()
     fb.next()  
     fb.next()
     fb.next()          
     for line in fb:
       stringtf=line.replace(";"," ")                    # new 20/01/16
       stringtf=stringtf.replace("\",\"",";")
       stringtf=stringtf.replace("\",,\"",";;")
       stringtf=stringtf.replace("\",,,\"",";;;")
       stringtf=stringtf.replace(",\"",";")
       stringtf=stringtf.replace("\",",";")
       stringtf=stringtf.replace("\"","")
       stringtf=stringtf.replace(","," ")
       stringt=stringtf.split(";")
       test_st=stringt[2]
       tno=test_st.find(rg)
       if tno == -1:
          key=stringt[1]
          mcent=stringt[2]
          scentno=stringt[3]
          scent=stringt[4]
          templ=[]
          templ.append(scentno)
          templ.append(mcent)
          templ.append(scent)
          if key in c12_read.keys():
             ml=c12_read[key]
             ml.append(templ)
             c12_read[key]=ml 
          else: 
             t2=[]
             t2.append(ml)
             c12_read[key]=ml 
#
   return(c12_read)
#
##############################################################################################################################################################################################################
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
# output:          flist :  list of files fitting the time and char matching criteria                            
#
##############################################################################################################################################################################################################
#
def listing_build(ah,tsp1):
# 
  ah2=ah[5]                                   # extract dir path for the files 
  now = dt.datetime.now()
  ago = now-dt.timedelta(minutes=tsp1)
  flist=[]
  for root, dirs,files in os.walk(ah2):       # generate list of filenames from the processed directory.
                                              # utilises the 'OS:walk' function
      for fname in files:
          path = os.path.join(root, fname)
          st = os.stat(path)    
          mtime = dt.datetime.fromtimestamp(st.st_mtime)
          if mtime > ago:
              flist.append(path)
  return(flist)
#
##############################################################################################################################################################################################################
#
# subroutine name:  fprocess
#
# purpose:          1.  Decode and construct listing of sections 1, 2 & 3
#                       of each MHSR* file listed by listing_build.   
#                                
# 
# input:           indct    :  Dictionary of lists of the form :-
#                              [['Datatype','storage_datasets','header file','elements_index','processed Directory',
#                              [[element index 1[,]element index 2],[.....]],[MHSR file listing]]]   
#                              defined sequences.
#                  c11_dict :  Dictionary of table C11 values
#                  c12_dict :  Dictionary of table C12 values  
#                  errpf    :  Dictionary of encoding / decoding errors (presented as a list)
#
# calls:           sum_bin 
#                  
# output:          brdict3 :  Dictionary of lists:- 
#                             MHSR FILENAME - Bulletin-No{['TTAAii CCCC','BUFR Edition','BUFR Master Table No','Originating Centre','Originating Sub-Centre',
#                                                           'Year','Month','Day','Hour','Minute','Second',['Encoding D Sequence']]
#                             example =>                                   
#                             /var/moods/bulletins/processed/SNT1/MHSR.R2D17236.T091005.SNT1.s450-12   ['IOPK01 AMMC', 4, 0, 'Melbourne', 'N/A', 2017, 8, 8, 4, 40, 0, ['315003']]                            
#
##############################################################################################################################################################################################################
#
def fprocess(indct,c11_dict,c12_dict,errpf):
#  
  brdict={}
  brdict2={}
  brdict3={}
  nm={}
  nrrf = -1 
  si=[0,1,4,6,7,8,9,10,12]
# 
  for key in indct.keys():                                               
     dty=indct[key][0][0]                                           # data type                              
     data=indct[key][0][8]                                          # last entry in the list will be a listing of the MHSR data associated with the header ranges etc.
     hddata=indct[key][0][7]                                        # header ranges in hex 
     hfile=indct[key][0][1]                                         # header file 
     hrange=indct[key][0][6]                                        # element index sequences 
     lend=len(data)
     n=0                                                            # seperate the list of MHSR data which has been appended to each entry 
     for y in range(0,lend):
        n=n+1 
        if len((data[y].strip())) == 0:
           break 
        filein=data[y] 
        (brdict,errpf,nrrf)=sum_bin(hfile,dty,filein,c11_dict,c12_dict,errpf)
        if nrrf > -1: 
           for key in brdict.keys():
               tlist = brdict[key]   
               tlist.insert(0,dty)
               tlist.append(hddata)
               tlist.append(hrange)    
               brdict2[key]=tlist 
               tlist=[]             
#    
  for key in brdict2.keys():                                        # brdict contains a decode of the MHSR BUFR file headers, 1 per message ending with the BUFR encoding sequence    
    qlist=brdict2[key] 
    key2=key.split("/")
    key3=key2[-1:][0]                                        
    brdict3[key3]=qlist                                                # replace the dict entry with the new list, which includes the expanded encoding sequence
  for key in brdict3.keys():  
     idh=brdict3[key][1].split(' ')
     idhf=idh[0]
     hident=binascii.b2a_hex(idhf)
     hident1=bin(int(hident,16))[2:]
     hidentf=int(hident1,2)
     hrange=brdict3[key][13]
     hrlen=len(hrange)
     passh = 0
     for xh in range(0,hrlen):
        trange=hrange[xh]      
        if hidentf >= trange[0] and hidentf <= trange[1]:
           passh = 1
           break   
     if passh == 0:
        dlist=errpf['TTAAii not in prescibed header range ']
        key2=key.split('/')
        key3=key2[-1:][0].split('-')
        keyf=key3[0]
        obn=key3[1]
        ttaaii= brdict3[key][1]   
        dlist2=[] 
        dlist2.append(keyf)
        dlist2.append(obn) 
        dlist2.append(ttaaii) 
        dlist.append(dlist2)
        errpf['TTAAii not in prescibed header range ']=dlist
        brdict3.pop(key,None)
#
  nm={}
  for key in brdict3.keys():                                                   # iterate through the bulletin entries     
     emcseq=brdict3[key][13]                                                   # establish the bulletin encoding sequence (expanded)  
     if len(emcseq) == 0:
        tlx = errpf['FXY Encoding sequence unobtainable ']  
        key6=key.split('/')
        key7=key6[-1:][0].split('-')
        key7f=key7[0]
        obn=key7[1]
        ttaaii= brdict3[key][1]
        dlist3=[]
        dlist3.append(key7f)
        dlist3.append(obn) 
        dlist3.append(ttaaii) 
        tlx.append(dlist3)   
        errpf['FXY Encoding sequence unobtainable '] = tlx 
        continue
     lenenc=len(emcseq)                                                        # length of the encoding sequence 
     elseq = brdict3[key][14]                                                  # expanded retrival element indexes (may be 1+)    
     lenel=len(elseq)                                                          # no of entries in the element index list of lists    
     tlap = []
     tlap2 = []
     tlap3 = []
#
  for key in brdict3.keys():
     tlist = brdict3[key][14]
     lentl = len(tlist)
     for xp in range(0,lentl):
        tlist2=tlist[xp]
        lentl2=len(tlist2)
        alist=[]
        for xg in range(0,lentl2):
            if tlist2[xg].isalnum():
               alist.append(tlist2[xg])
        tlist[xp] =  alist  
     brdict3[key][14] = tlist
#   
  for key in brdict3.keys():  
     f1list = brdict3[key][12]                                                   # bulletin BUFR encoding sequence
     f2list = brdict3[key][14]                                                   # retrieval index sequences (nested lists)
     lenel = len(f2list)
     for zx in range(0,lenel):         
         eh = [(i,j) for i,j in zip(f2list[zx],f1list) if j != i]       
         if len(eh) == 0:           
            break                                                                 # len(eh) == 0 exact match bewteen the encoding seq and the index 
         elif zx == lenel -1 and len(eh) > 0: 
            nm[key] = brdict3[key]
#          
  return(nm,errpf)
#       
#############################################################################################################################################################################################################
#
# subroutine name: read_config
#
# purpose:         read config file. Output details to a dict of lists.    
#
# input:           cnfgin     :      config file name
# 
#
# output:          fdict      :      output dictionary of lists of the form 
#                                    odict[job stream]{[['data type','storage_datasets','header_file','element_index',bufr_localSeq,'processed dir],[..]]}   
#              
#  
#############################################################################################################################################################################################################
#

def read_config(cnfgin,trigger):
 olist=[]
 fdict={}
 TFIG='NOW SERVING => '
 with open(cnfgin,'r') as fg:
    for line in fg:   
       if line.find(trigger) >= 0:     
          line1=line.strip('\n')
          line2=line1.rstrip()
          line3=line2.lstrip()  
          line4=' '.join(line3.split()) 
          line5=line4.split(' ')
          line6=line5[1:]                                             # key to the dict entry will be the data stream ie GTS1     
          fsp1=line6[-1:]
          fsp2=fsp1[0].replace('//','/')
          if fsp2[-1:] == '/':
            fsp3=fsp2[:-1]
          else:
            fsp3=fsp2     
          fsp4=fsp3.split('/')
          keyf=fsp4[-1:][0]  
          if keyf in fdict.keys():                   
             tlist=fdict[keyf]    
             tlist.append(line6)
             fdict[keyf]= tlist
          else:
             tlist = []
             tlist.append(line6)
             fdict[keyf] = tlist  
    fg.close() 
#
 return(fdict)   
#
#
##############################################################################################################################################################################################################
#
# subroutine name: web_dout
#
# purpose:         Output results to a html page.             
#
# input:           wcd1     :  Dictionary of lists for incoming D sequences which do not have an equivalent index entry 
#                            
#                  wcd2     :  Dictionary of lists containg error messages.
#
# output:          write out to D_sequence_comparison.html
#              
#
##############################################################################################################################################################################################################
#
def web_dout(wcd1,wcd2):
#   
    fdx='D_sequence_comparison.html'
    with open(fdx,'w') as fxw: 
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
         fxw.write(('%s\n') % ("<caption style=\"background-color:#ebfafa\"><b> Data rejected with no matching Element Index </b></caption>"))  
         fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\"><b> MHSR file name </b></td>"))
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"5%\"><b> Ob No </b></td>"))  
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"15%\" ><b> TTAAii CCCC </b></td>"))
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"15%\" ><b> YYYY-MM-DD hh:mm:ss</b> </td>"))
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"25%\"><b> Originating Centre </b></td>"))
         fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\" ><b> FXY (Encoding Sequence) </b></td>"))      
         fxw.write(('%s\n') % ("</tr>"))  
         n = 1
         for key in wcd1.keys():   
             if n%2 == 0: 
                  fxw.write(('%s\n') % ("<tr style=\"background-color:#e6fff7\" width=\"100%\" >")) 
             else:
                  fxw.write(('%s\n') % ("<tr width=\"100%\">")) 
             n = n+1
             linew=wcd1[key]
             tai = linew[1]     
             kout=key.split("/")             
             kf=kout[-1:][0]  
             kf2=kf.split("-") 
             obn=kf2[1]
             kff=kf2[0] 
             ens = " ".join(map(str,linew[12]))  
             ens2=ens+'<br>'                    
             mnf=str(linew[7])
             if len(mnf) < 2:
                mnf = "0" + mnf 
             dyf=str(linew[10])
             if len(dyf) < 2:
                dyf = "0" + dyf  
   
             hrf=str(linew[9])
             if len(hrf) < 2:
                hrf = "0" + hrf  
 
             mnt=str(linew[10])
             if len(mnt) < 2:
                mnt = "0" + mnt  

             scf = "00"    
              
             dmd = str(linew[6])+"-"+str(mnf)+"-"+str(dyf)  
             hms = str(hrf)+":"+str(mnt)+":"+str(scf)                      # ddyymm: hhmmss   
             ogc= linew[4] 

             el1="<td style=\"font-size:17px\" width=\"20%\" >"+str(kff)+"</td>" 
             fxw.write(('%s\n') % (el1))

             obo="<td style=\"font-size:17px\" width=\"5%\" >"+obn+"</td>" 
             fxw.write(('%s\n') % (obo))

             taio="<td style=\"font-size:17px\" width=\"15%\">"+tai+"</td>" 
             fxw.write(('%s\n') % (taio)) 
  
             fdm = "<td style=\"font-size:17px\" width=\"15%\" >"+dmd+" "+hms+"</td>"   
             fxw.write(('%s\n') % (fdm)) 
    
             ogcm = "<td style=\"font-size:17px\" width=\"25%\" >"+ogc+"</td>" 
             fxw.write(('%s\n') % (ogcm)) 
             
             encf="<td style=\"font-size:17px\" width=\"20%\">"+ens2+"</td>" 
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
         fxw.write(('%s\n') %  ("<caption style=\"background-color:#ebfafa\"><b> Decoding Errors </b></caption>"))
         fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
         fxw.write(('%s\n') % ("</table >"))
         fxw.write(('%s\n') % ("<br>"))
#
         for key in wcd2.keys():
             if key != 'Unable to find BUFR in message' and key != 'Unidentified D sequence in Decode of Element Index':    
                if len(wcd2[key]) > 0:  
                    fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                    fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"35%\"><b> Description</b></td>"))   
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"35%\"><b> MHSR file name </b></td>"))
                    fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"10%\"><b> Ob No </b></td>"))
                    if key == 'Unidentified D sequence in Decode of MHSR file':
                        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\"><b> D Sequence</b></td>")) 
                    else:
                        fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\"><b> TTAAii CCCC</b></td>")) 
                    fxw.write(('%s\n') % ("</tr>")) 
                    tderr="<td style=\"font-size:17px\" width=\"35%\" >"+key+"</td>" 
                    tderr2="<td style=\"font-size:17px\" width=\"35%\" >  </td>"
                    for y in range(0,len(wcd2[key])):
                        mhsr="<td style=\"font-size:17px\" width=\"35%\" >"+wcd2[key][y][0]+"</td>" 
                        obfn="<td style=\"font-size:17px\" width=\"10%\" >"+wcd2[key][y][1]+"</td>"
                        ttaan="<td style=\"font-size:17px\" width=\"20%\" >"+wcd2[key][y][2]+"</td>" 
                        fxw.write(('%s\n') % ("<tr>"))
                        if y == 0:                                            
                           fxw.write(('%s\n') % (tderr)) 
                           fxw.write(('%s\n') % (mhsr))
                           fxw.write(('%s\n') % (obfn)) 
                           fxw.write(('%s\n') % (ttaan))
                           fxw.write(('%s\n') % ("</tr>"))   
                        else:  
                           fxw.write(('%s\n') % (tderr2))  
                           fxw.write(('%s\n') % (mhsr))
                           fxw.write(('%s\n') % (obfn)) 
                           fxw.write(('%s\n') % (ttaan))
                           fxw.write(('%s\n') % ("</tr>"))                                   
                    fxw.write(('%s\n') % ("</table>"))
                    fxw.write(('%s\n') % ("<br>"))
             else: 
                if key == 'Unable to find BUFR in message':
                   if len(wcd2[key]) > 0:     
                      fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                      fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"35%\"><b> Description </b></td>"))   
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"35%\"><b> MHSR file name </b></td>"))
                      fxw.write(('%s\n') % ("</tr>")) 
                      for z in range(0,len(wcd2[key])):
                          tderr="<td style=\"font-size:17px\" width=\"35%\" >"+key+"</td>" 
                          tderr2="<td style=\"font-size:17px\" width=\"35%\" >  </td>"   
                          mhsr="<td style=\"font-size:17px\" width=\"35%\" >"+wcd2[key][y][0]+"</td>"   
                          if z == 0:                                       
                              fxw.write(('%s\n') % (tderr)) 
                              fxw.write(('%s\n') % (mhsr))
                              fxw.write(('%s\n') % ("</tr>")) 
                          else:
                              fxw.write(('%s\n') % (tderr2)) 
                              fxw.write(('%s\n') % (mhsr))   
                              fxw.write(('%s\n') % ("</tr>")) 
                      fxw.write(('%s\n') % ("</table>"))
                      fxw.write(('%s\n') % ("<br>"))  
                elif key == 'Unidentified D sequence in Decode of Element Index':
                   if len(wcd2[key]) > 0:     
                      fxw.write(('%s\n') % ("<table style=\"width:100%\" >"))
                      fxw.write(('%s\n') % ("<tr style=\"background-color:#ebfafa\">"))
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"35%\"><b> Description </b></td>"))   
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"15%\"><b> Data Stream </b></td>"))   
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"20%\"><b> Element Index </b></td>"))
                      fxw.write(('%s\n') % ("<td style=\"font-size:17px\" width=\"30%\"><b> D sequence </b></td>"))
                      fxw.write(('%s\n') % ("</tr>")) 
                      for z in range(0,len(wcd2[key])):
                          tderr="<td style=\"font-size:17px\" width=\"35%\" >"+key+"</td>" 
                          tderr2="<td style=\"font-size:17px\" width=\"35%\" >  </td>"   
                          dst="<td style=\"font-size:17px\" width=\"35%\" >"+wcd2[key][z][0]+"</td>"  
                          dhf="<td style=\"font-size:17px\" width=\"35%\" >"+wcd2[key][z][1]+"</td>"
                          dlistf= " ".join(wcd2[key][z][2])
                          dhs="<td style=\"font-size:17px\" width=\"35%\" >"+dlistf+"</td>"   
                          if z == 0:                                       
                              fxw.write(('%s\n') % (tderr)) 
                              fxw.write(('%s\n') % (dst)) 
                              fxw.write(('%s\n') % (dhf))
                              fxw.write(('%s\n') % (dhs))
                              fxw.write(('%s\n') % ("</tr>")) 
                          else:
                              fxw.write(('%s\n') % (tderr)) 
                              fxw.write(('%s\n') % (dst)) 
                              fxw.write(('%s\n') % (dhf))
                              fxw.write(('%s\n') % (dhs))
                              fxw.write(('%s\n') % ("</tr>")) 
                      fxw.write(('%s\n') % ("</table>"))
                      fxw.write(('%s\n') % ("<br>"))          
    fxw.close()  
    return() 
#   
#
##############################################################################################################################################################################################################
#
# subroutine name: read_hdr
#
# purpose:                Read the First and Last TTAAii from the header file for that data type.
#                         Convert the TTAAii to HEX values            
#
# input:                  def_file -  header file name
#                         sname    -  data type             
#                          
#                  
# output:                lflist => [[TTAAii(start) (HEX) , TTAAii(end) (HEX)], [TTAAii(start) (HEX) , TTAAii(end) (HEX)],[TTAAii(start) (HEX) , TTAAii(end) (HEX)].....]
#              
#
##############################################################################################################################################################################################################
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
#############################################################################################################################################################################################################
#
# Main calling function
#
# create listing of the directorys / files needed to proceed.
#
# MHSR files in data source directory
#
# 
#############################################################################################################################################################################################################
#
#
cfin='config_table_NEW1.txt'
#
osin = os.environ['MACHINE']
cnfg=read_config(cfin,osin)  
#
elist=[]
elist2=[]
brdictxf={}
#
# errw => dictionary to capture decoding errors
#
errw={}
errw['Unable to identify BUFR Edition']=[]
errw['Binary Read error in aquiring day']=[]
errw['Binary Read error in aquiring hour']=[]
errw['Binary Read error in aquiring minute']=[]
errw['Binary Read error in calculating no of data subsets']=[] 
errw['FXY Encoding sequence unobtainable '] = []
errw['Unidentified D sequence in Decode of MHSR file']=[]
errw['Unidentified D sequence in Decode of Element Index']=[]
errw['Incorrectly encoded value for section 3 length (section 2 not present) -  BUFR Edition 3']=[]
errw['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 3']=[]
errw['Incorrectly encoded value for section 3 length (section 2 not present) - BUFR Edition 4']=[]
errw['Incorrectly encoded value for section 3 length (section 2 present) - BUFR Edition 4']=[]
errw['TTAAii not in prescibed header range ']=[]
errw['Unable to find BUFR in message']=[]
#  
for key in cnfg.keys():    
    tdir =cnfg[key][0][4]                                             # definition of the tables directory from the config file  
    break                                                             # taken on first loop 
#
mldir = os.listdir(tdir)                                              # listing of the tables dir (tdir)
#                                                                                    
lenml = len(mldir)
#
for x in range(0,lenml):                                             # establish location and name of common code tables C11 & C12  
   if mldir[x].find("Common_C11") > -1:                              # these are required in decoding to identify issuing centres 
      c11_file=tdir+mldir[x]
      (c11_dict)=read_c11(c11_file)                                  # Read file C11 into dict  
   elif mldir[x].find("Common_C12") > -1:
      c12_file=tdir+mldir[x]
      (c12_dict)=read_c12(c12_file)                                  # Read file C12 into dict   
#
#
#################################################################################################################  
#
xp=240                                                         # constant (in mins) used to calculate the 
                                                               # time span of the data to be reviewed .
plist=[]
hdict={}
#
for key in cnfg.keys():                                             
   df = cnfg[key]
   ldf=len(df)
   for yx in range(0,ldf):                                      # compile listing of data in dir and header file in HEX     
      hdn=df[yx][1]
      din=df[yx]    
      sn = df[yx][0]
      slist=listing_build(din,xp)                               # take listing of the MHSR files fitting the date / time criteria
      plist=read_hdr(hdn,sn) 
      df[yx].append(plist)
      df[yx].append(slist)                                      
   cnfg[key]=df                                                 # add header file ranges and MHSR listing to the cnfg entry
#                                                               # for that data type
for key in cnfg.keys():
   dfl=len(cnfg[key])
   for xv in range(0,dfl):
       ein = cnfg[key][xv] 
       dtype=cnfg[key][xv][0]
       elist=cnfg[key][xv][3]   
       elout=ind_read(elist)                                        # read in retrieval index(s) for a specific data type   
       ein.insert(6,elout)                                          # add entry to cnfg entry for that data type 
       cnfg[key][xv]=ein 
#
eldict2={}
#
(brdictxf,erryx)=fprocess(cnfg,c11_dict,c12_dict,errw)              # final processing of the data including decode of the MHSR data
#
web_dout(brdictxf,erryx)                                            
#
#    
##############################################################################################################################################################################################################