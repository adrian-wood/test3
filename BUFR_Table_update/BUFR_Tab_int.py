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
####################################################################################################################################################################
#       Name -      BUFR_Tables.py
# 
#       Author -    Richard Weedon
#
#       Date -      11th April 2016..
#
#       Description. 
#    1. Add new Version of Global Table B (requires txt version of Table B, extracted from the WMO zip folder)
#    2. Add new Version of Global Table D (requires txt version of Table D, extracted from the WMO zip folder)
#    3. New Table B Local Entry (will be added to current operational table)
#    4. New Table D Local Entry (will be added to current operational table)
#    5. Compare new and old Table B revisions (requires txt version of Table D, extracted from the WMO zip folder)
#    6. Compare new and old Table D Revisions (requires txt version of Table D, extracted from the WMO zip folder)
#    7. Add new version of BUFR Codefigs.
#    8. Expand WMO D Sequence to constituent B Descriptors.
#    9. Expand WMO D Sequence and produce outline for a MetDB element Index.
#   10. Exit / Quit   
#    
#   Note:
#   Options 1 - 4 & 6 - 9 will use wget to extract the latest versions of the BUFR and / or 
#   Codefig tables from the WMO Code Tables Web Site. In addition the current operational versions 
#   of the BUFR and Codefig tables will be copied (scp) from MDBDB-PREPROD. To complete this
#   the user must have access to the moodsf account on the MetDB. 
#
#   Option 10 will run a full decode of all files (MHSR) found in the depository directory (~/decode_in/)
#  
#
#
#       Subroutine description  .
#
#         Name                                            Purpose   
#       
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------  
#             
#     table_add(Tno)                    Called by Menu(), this will initiate a general prompt
#                                       for the name of the input files etc. 
#
#                                       input   - (Tno) option identifier (1 - 10)  
#
#                                       output  - Varies according to the option called.
#                                       tablex - name and location of BUFR XML file (Either B or D)
#                                       tableo - name and location of BUFR txt file (Either B or D)
#                                       tableq - name and location of BUFR Table C  
#                                       seq_an - Table B or D sequence identifier    
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------  
#
#     add_entry(w,v)                    The manual entry of either Table B Descriptors and 
#                                       / or Table D Sequences.
#
#                                       add_entry(w,rev)
#
#                                       w    -   switch to define the table updated (either B or D)
#                                       rev  -   revision no of current BUFR table
#
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     finaldictb(masterl,masterg)      Will merge master dicts output by GetDatb and read_in_tableb into a single final dict. 
#                                      Where entrys from the new (WMO) txt tables are found in the existing tables a new revision will 
#                                      be added. Where new entries are found without a corresponding entry in the existing table 
#                                      a new (singlar) entry will be made. Each entry in the merged dict is then sorted by the
#                                      revision no (in ascending order). The presence of previous versions is indicated by placing 
#                                      a "+" againt the latest version no of any entry.
#
#                                      input   - masterl - Dictionary (containing nested lists) of the existing operational entries.
#                                      input   - masterg - Dictionary new entries parsed from the WMO xml tables.
#                                      output  - fdict   - Merged dict derived from the local and global dicts.
#
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------  
#
#     finaldictd(masterl,masterg)      Will merge master dicts output by GetDatd and read_in_tabled into a single final dict. 
#                                      Where entrys from the new (WMO) txt tables are found in the existing tables the new revision will 
#                                      replace the old. Where new entries are found without a corresponding entryb in the existing table 
#                                      a new (singlar) entry will be made. Each entry in the merged dict is then sorted by the
#                                      revision no (in ascending order).
#
#                                      input  - masterl - Dictionary (containing nested lists) of the existing operational entries.
#                                      input  - masterg - Dictionary new entries parsed from the (WMO) txt tables.
#                                      output - fdict   - Merged dict derived from the local and global dicts.
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#   
#     getDatB(tablenb)                 Will parse and read into a dictionary the contents of the new (WMO) txt version
#                                      of BUFR tableB. Each descriptor key will reference a series of lists as outlined below -
#
#                                      dict[key]=[Descriptor Name,unit,scale,ref,width,cunit,cscale,cwidth]
#
#                                      input -   tablenb   - Name of the new BUFR table B ((WMO) txt).
#                                      output -  tableout  - Master dict for the new BUFR Table B (excluding local descriptors)
#                                                version   - Revision No of the new BUFR table  
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#     
#     getDatD(tablend)                 Will parse and read into a dictionary the contents of the new (WMO) txt version
#                                      of BUFR tableD. Each descriptor key will reference a series of lists as outlined below -
#
#                                      dict[key]=[Descriptor1,Descriptor2,Descriptor3,Descriptor4,]
#
#                                      input -   tablend   - Name of the new BUFR table D ((WMO) txt).
#                                      output -  tableout  - Master dict for the new BUFR Table D (excluding local sequences)
#                                                version   - Revision No of the new BUFR table  
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
# 
#     read_in_tableb(tablename)        This subroutine will read into memory the contents
#                                      of the operational version of BUFR tableB. The 
#                                      file will contain multiple versions of the same 
#                                      entry which must be preserved and will include local
#                                      descriptors. The data will be read into a dict 
#                                      of lists which are in turn listed in the master lists. 
#                                      An entry for any descriptor will be of the form -
#
#                                      dict[010114][[rev(1),Descr,unit,scale,ref val,data width],[rev(2),Descr,unit,scale,ref val,data width]]                                            
#                                
#                                      The nested list structure is sorted in ascending order
#                                      of the revision no before being assigned to the dict. 
#                                
#                                      input -   Tablename - Name of the operation BUFR table B (txt format).
#                                      output -  Master    - Master dict for current (operational) BUFR
#                                                rev_line  - Revision No of the current operational table            
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
# 
#     read_in_tabled(tablename)        This subroutine will read into memory the contents
#                                      of the operational version of BUFR tableD. Unlike TableB 
#                                      the file will not contain multiple versions of the same 
#                                      entry. Each entry will comprise a single list of the form -
#                                      dict[307079]["Title","count",Descriptor1,Descriptor2,.....]  
#
#                                      input -   Tablename - Name of the operation BUFR table B (txt format).
#                                      output -  Master    - Master dict for current (operational) BUFR
#                                                rev_line  - Revision No of the current operational table
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#               
#     output_b(f_dict,N_ver,O_ver)     Will output the final (merged) dict in the current 
#                                      operational format. 
#                                
#                                      input -   f_dict  - final merged dictionary (containing nested lists).
#                                                N_ver   - Local Version of the current operational tables.
#                                                N_ver   - WMO Version of the current operational tables.   
#                                      output -  The filename of the final output will be of the form  -
#                                                BUFR_TableB_YYYY_MM_DD_HH:MM:SS.txt.
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#  
#     output_d(f_dict,N_ver,O_ver)     Will output the final (merged) dict in the current operational format. 
#                                
#                                      input  -  f_dict  - final merged dictionary (containing nested lists).
#                                      N_ver  -  Local Version of the current operational tables.
#                                      N_ver  -  WMO Version of the current operational tables .   
#                                      output -  The filename of the final output will be of the form  -
#                                      BUFR_TableD_YYYY_MM_DD_HH:MM:SS.txt.
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#  
#     comp_versions((nvers,overs,table,wmo_rev,loc_rev,hdiri) 
#
#                                      Run comparison of two dicts constructed from the new and old versions 
#                                      of BUFR Table B or D.  
#
#                                      input - nvers - new version of the BUFR Table
#                                              overs - old version of the BUFR table
#                                              table - Switch to signify the table type (B or D)                       
#
#                                      output -TableD_Comparison_YY-MM-DD_HH:MM:SS.txt 
#                                              TableB_Comparison_YY-MM-DD_HH:MM:SS.txt                                            
#
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#         
#     read_oldfig(tablename)           Read in the current (operational) BUFR Codefigs to a dict.  
# 
#                                      input- tablename - Current (operational ) version of codefigs.
#                             
#                                      output   fin_dict   - Dict of lists containing current (MetDB) CodeFig tables.
#                                                            The dict will be of the structure -
#
#                                                            Dict[Copde Table No]=[[Title][Entry No, Entry],[Entry No, Entry]...]
#         
#                                                            rev_no        - Revision No of the current version of codefigs 
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
# 
#     read_newfig(tablename)           Read in the WMO version of BUFR Codefigs (.txt) to a dict.  
#
#                                      input:   Tablename     - WMO Codefigs (.txt) 
#     
#                                      output:  fin_dict   - Dict of lists containing current (MetDB) CodeFig tables.
#                                                            The dict will be of the structure -
#
#                                                            Dict[Copde Table No]=[[Title][Entry No, Entry],[Entry No, Entry]...]
#         
#                                                            rev_no        - Revision No of the current version of codefigs 
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
# 
#     mdict(masterld,mastergd)         merge WMO and MetDB versions of the BUFR Codefig tables
#
#                                      input:      Dict  masterld     -  local version of the  codefig tables
#                                                  Dict  mastergd     -  WMO Version (New) version of the code tables    
#
#                                      output:  -  Merged data in a dictionary     
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     out_codefig(mfdict)              output BUFR Codefig files
#
#                                      input:       mfdict (dict output by mdict)     -  Merged codefig tables
#
#                                      output:      CodeFig_YY-MM-DD_HH:MM:SS.txt 
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     link_common_tables(mfdict)       Cross refence the entries in the merged data dictionary for codefigs
#                                      with the Common Code Tables 
#    
#                                      input:      mfdict     -  Merged codefig tables
#
#                                      output:     mdict_out  -  Merged codefig tables with entries from the common code tables.                                                                                                                  
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     out_codefig(mfdict)              output BUFR Codefig tables to file
#
#                                      input:  mfdict     -  Merged codefig tables
#
#                                      output: BUFR CodeFig Table - CodeFig_YY-MM-DD_HH:MM:SS.txt     
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     comp_code_dicts                  comparison of the new and old codefig files
#
#                                      input:  dict1     -  Current MetDB codefig tables (dictionary of lists)
#                                              dict2     -  New Code Fig tables downloaded from the WMO Web Pages
#
#                                      output: Nil   
#
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     rep_d                            Expand all D sequences, replacing nested D sequences with full listings of B Descriptors
#
#                                      input:  odictn  -  Dict of D Sequences.
#
#                                      output: odictf  -   Dictionary containing expanded D sequence.
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     print_list                       Subroutine called by "straight" to identify and replace nested lists. 
#                                      The instance method "isinstance" will return TRUE if the object matches the type
#                                      specified , in this case "List".Therefore a nested list such as -
#
#                                            ["23","24","25",["26","27","28"],"29","30"]
#                                                    will be transformed to 
#                                              ["23","24","25","26","27","28","29","30"]         
#
#                                     input:    the_list    -  list which may (or may not) contain nested lists
#
#                                     returns:  dn          -  output lists
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     straight                         Will search for nested lists (within the main list) and merge the nested 
#                                      data at the right point. The end result will be one continous list. Note this 
#                                      subroutine includes a call to the subroutine print_list 
#
#                                      input:   idict -  expanded D sequence returned by rep_d. 
#
#                                      returns:  tempd - dictionary of lists corresponding to an expanded Table D Sequence
#                                                       cross referenced against Tables B & C.
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     read_tablec                      read in the contents of BUFR Table C to a dictionary of lists. 
#                                      The dictionary will be of the format -
#
#                                           tabc{key :[ 'Descriptor' ,'Details',' ',' ']}
#
#                                      input:     cin  -  text version of Table C
#
#                                      returns:   tabc -  dictionary of lists corresponding to Table C
#
#   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     cross_ref                        cross_reference an expanded D sequence against BUFR Tables B & C
#
#                                      input:  bdictin    -  dictionary of lists containing Table B
#                                              ddictin    -  dictionary of lists containing Table D
#                                              cdictin    -  dictionary of lists containing Table c
#
#                                      returns: tempd     -  dictionary of lists corresponding to an 
#                                                            expanded Table D Sequence cross 
#                                                            referenced against Tables B & C. 
#  
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     out_seq                          Output Expanded D Sequence
#
#                                      Input: fdict      -   Expanded D Sequence , cross referenced against Table C. 
#                                                            Note this is output from cross_ref
#                                             seqID      -   D Sequence which has been expanded.
#
#                                      returns:              Nil.  
#
#                                      Output:               D_Sequence_Sequence_ID_output_YYDDMM_HHMM.txt 
#   ----------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#
#     index_cr                         Output skeleton of the elements index for a specific D sequence
#
#                                     input:  fdict   -  Expanded D Sequence , cross referenced against Table C. Note this is output from cross_ref
#                                             seqID   -  D Sequence which has been expanded.
#
#                                     output:           Element_Index_SeqID_output_YYMMDD_HHMM.txt  
#             
####################################################################################################################################################################
# subroutine name: table_check
#
# purpose:         check for the presence of the tables. Failure to locate the required table(s) will result in the program exit.
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
####################################################################################################################################################################
#
def table_check(opt,hdir,tablexf,tableof,tableqf):
#  
#   
   if opt == "1" or opt == "3" or opt == "6":
#
       if not os.path.isfile(tablexf):
           print "BUFR_Tables: Global BUFR TableB not found at =>  ",tablexf
           exit() 
       if not os.path.isfile(tableof):
           print "Local BUFR TableB not found at =>   ",tableof
           exit()  
#       
   elif opt == "2" or opt == "4" or opt == "7":
       spathx=hdir+"/BUFR_d/"+tablexf
       spatho=hdir+"/op/"+tableof 
       if not os.path.isfile(tablexf):
           print "BUFR_Tables: Global BUFR TableD not found at =>  ",spathx
           exit()
       if not os.path.isfile(tableof):
           print "Local BUFR TableD not found at =>   ",spatho
           exit()  
#  
   elif opt == "5":
       if not os.path.isfile(tablexf):
           print "BUFR_Tables: Global Common Code Table not found at =>  ",tablexf
           exit()
       if not os.path.isfile(tableof):
           print "Local Common Code Table not found at =>   ",tableof
           exit()  
#  
   elif opt == "8" or opt == "9":
       if not os.path.isfile(tablexf):
           print "BUFR_Tables: Global BUFR TableD not found at  =>  ",tablexf
           exit()
       if not os.path.isfile(tableof):
           print "BUFR_Tables: Global BUFR TableB not found at =>   ",tableof
           exit()
       if not os.path.isfile(tableqf):
           print "BUFR_Tables: Global BUFR TableC not found at =>   ",tableqf
           exit() 
   return()
#      
####################################################################################################################################################################
# subroutine name: add_entry
#
# purpose:         Manually add new entrys to either BUFR Tables B or D
#
# input:           w            -   switch to define the table updated (either B or D)
#                  rev          -   revision no of current BUFR table
#
# output:          final        -   New Descriptor(s) / sequence(s) as a BUFR DICT  
#
#####################################################################################################################################################################
#
def add_entry(w,rev,master_l):
#
#                                               
#
 if w == "B":                                                                # select table B
   y=raw_input("No of Table B Descriptors to be created? :  ")
   y2=int(y)
   for x in range(0,y2):
     row=[]
     row2=[]
     xn=x+1
     xns=str(xn)   
     desn="FXY of Descriptor "+xns+" : "   
     while True:                                                  # prompt for the details of the new descriptor 
       fxy=raw_input(desn)
       questionfxy= fxy+" - entered for descriptor identifier\n" 
       print questionfxy
       q2p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Identifier or Q to Quit) ")
       q2p=q2p.upper()
       if q2p == "Y" or q2p == "Q":
         if q2p == "Y":
           break 
	 if q2p == "Q":
           exit()    
	 	 	 
     descf="Description of "+fxy+" (60 Chars or less):  "       # FXY   
     while True: 	 
       desc=raw_input(descf)  
       questiondesc= " "+desc+" - entered for description of "+fxy+"\n"
       print questiondesc
       q3p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Description or Q to Quit) ")
       q3p=q3p.upper()
       if q3p == "Y" or q3p == "Q":
         if q3p == "Y":
           break 
	 if q3p == "Q":
           exit()   
	 
     print "++++++++++++\n" 	 
     unitf="Unit of "+fxy+" : "  
     while True:                                
       unit=raw_input(unitf)  
       questionunit= " "+unit+" entered as the unit of "+fxy+"\n"
       print questionunit
       q4p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Unit or Q to Quit) ")
       q4p=q4p.upper()
       if q4p == "Y" or q4p == "Q":
         if q4p == "Y":
           break 
	 if q4p == "Q":
           exit()   
	   
     print "++++++++++++\n"  
     scalef="Scale of "+fxy+" : "  
     while True:                                
       scale=raw_input(scalef)  
       questionscale= " "+scale+" entered as the Scale of "+fxy+"\n"
       print questionscale
       q5p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Scale value or Q to Quit) ")
       q5p=q5p.upper()
       if q5p == "Y" or q5p == "Q":
         if q5p == "Y":
           break 
	 if q5p == "Q":
           exit()   
	   
     print "++++++++++++\n"	    
     reff="Reference Value of "+fxy+" : "  
     while True:                                
       refer=raw_input(reff)  
       questionrefer= " "+refer+" entered as the Reference value for "+fxy+"\n"
       print questionrefer
       q6p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Reference value or Q to Quit) ")
       q6p=q6p.upper()
       if q6p == "Y" or q5p == "Q":
         if q6p == "Y":
           break 
	 if q6p == "Q":
           exit()  
	             	   
     print "++++++++++++\n"	                                     
     widthf="Data Width of (Bytes) "+fxy+" : "  
     while True:                                
       width=raw_input(widthf)  
       questionwidth= " "+width+" entered for the Data Width of "+fxy+"\n"
       print questionwidth
       q7p=raw_input( "is this correct?: (Y to proceed , N to re-enter the Data Width or Q to Quit) ")
       q7p=q7p.upper()
       if q7p == "Y" or q7p == "Q":
         if q7p == "Y":
           break 
	 if q7p == "Q":
           exit()                                                                                
     revf=float(rev)
     revf=revf+1
     revfs=str(revf)
     row.append(revfs)                                        # append new values into a single list
     row.append(desc)
     row.append(unit)
     row.append(scale)
     row.append(refer)
     row.append(width)
     row2.append(row)
     master_l[fxy]=row2
   return(master_l)    
 ##################### following applies to Table D #############################################################################     
 else:                                                                        # tableD entry                                            
   y=raw_input("No of D Sequences to be created: ")                           # prompt(s) for the details of the new sequence(s)
   y2=int(y)
   for x in range(0,y2):
     nt=raw_input("Title of the new Sequence, If no title enter \"NIL\" ")     # Prompt for title of the new sequence
     fxy=raw_input("Enter the sequence identifier (FXY):  ")                   # enter FXY for the sequence
     qa="No of descriptors in D sequence "+fxy+" :  "
     nd=raw_input(qa)                                                          # no of elements in the new sequence
     fxy2=fxy
     row=[]
     ndi=int(nd)
     row.append(nt) 
     row.append(nd)
     for z in range(0,ndi):                                                    # loop through the new sequence adding descriptors 
       zn=z+1
       zns=str(zn) 
       zd="Enter element no "+zns+" (FXY) :"
       while True:  
         seqd=raw_input(zd)                               
         questionwidth= " "+seqd+" entered as desciptor No "+zns+" in sequence "+fxy+"\n"
         print questionwidth
         di=raw_input( "is this correct?: (Y to proceed , N to re-enter the Data Width or Q to Quit) ")
         dirp=di.upper()
	 dlen=len(seqd)
         if dirp == "Y" or dirp == "Q":
           if dirp == "Y":
	      if dlen != 6:
	         print "Descriptor has incorrect length, re-enter\n"
	      else:
	         row.append(seqd)  
                 break   
           if dirp == "Q":
             exit()                                         
     master_l[fxy2]=row   
   return master_l  
#   
####################################################################################################################################################################
# subroutine name: final_dictb
#
# purpose:         Merge the Global (extracted from the WMO xml file) and the current operational versions of BUFR table B
#
# input:           masterl      -   Dict of lists for the current operational TableB (formatted as nested lists) example -
#                                   dict[010114][[rev(1),Descr,unit,scale,ref val,data width],[rev(2),Descr,unit,scale,ref val,data width]]
#      
#                  masterg      -   Dict of lists for version WMO TableB
#                                    
# output:          fdict        -   Dict of lists. Each version of a descriptor is nested within the list as a seperate list. 
#                             dict[010114][[rev(1),Descr,unit,scale,ref val,data width],[rev(2),Descr,unit,scale,ref val,data width]]      
#
####################################################################################################################################################################
#
def final_dictb(masterl,masterg,revm):
  fdict={}
  tlist=[]   
  revmc=str(revm)          
  for key in masterl.keys():                                             # loop through keys (FXY) in the local tables check if there is an equivalent in the wmo table  
     flist=[]   
     listl=[] 
     listg=[]
     listtmp=[]
     listl=masterl[key] 
     sl=len(listl)
     listl2=listl
     if sl > 1: 
       for z in range(0,sl):
          if z < sl-1:
            af1=listl2[z]
            af2=listl2[z+1]   
            aflist=list(set(af1[1:])- set(af2[y][1:]))
            if len(aflist) == 8:
               listl2.pop(z+1)  
       listl=listl2
# 
     if key in masterg:                                                  # if there is a corresponding FXY entry in the WMO list   
         listg=masterg[key][1:]                                          # extract the content of the list minus the revision no    
                                                                         # convert elements in the WMO list to upper case
         gstring=listg
         for q in range(0,8):         
           gul=gstring[q]
           gul2=gul.upper()
           gstring[q]=gul2      
         listg=gstring                                     
         sl2=len(listl) 
         listg.insert(0,revmc) 
         listg1 = listg[3:6]+listg[7:]
         listl2 = listl[0][3:6]+listl[0][7:]
         rep1 = []
         rep2 = []
         for xh in listg1:
             df = xh.replace(' ','')
             rep1.append(df)
         listg1 = rep1
         for xg in listl2:
             dg = xg.replace(' ','')
             rep2.append(dg)
         listl2 = rep2        
         flist=list(set(listg1)- set(listl2)) 
         flen=len(flist)
         if flen > 0: 
            print "final_dictb: listg ",listg,"\n"
            print "final_dictb: listl ",listl,"\n"
            print "final_dictb: listg1 ",listg1,"\n"
            print "final_dictb: listl2 ",listl2,"\n"  
            print "===========\n"
            listl.insert(0,listg)  
         fdict[key]=listl  
     else:                                                               # key FXY in the MetDB table does not appear in the WMO version  
                                                                         # May be local entry or may have been "retired"  
         for y in range(0,sl):                                   
            listm=listl[y]
            lenlm=len(listm)
            for z in range(0,lenlm):
               mstring=listm[z] 
               mul=mstring.upper()                                       # convert elements in the local list to upper case 
               listm[z]=mul  
            listl[y]=listm
         fdict[key]=(listl)                                  
#
############################################################
#       
  for key in masterg.keys():                                             # loop through global (WMO) dict and check for their presence in 
     gtemp=[] 	                                                         # fdict. 
     gtempx=[] 
     if key not in fdict:                                                # if the key is not in fdict              
         gtemp=masterg[key]
         for x in range(1,7):                                            # convert elements in the WMO list to upper case
            gstring=gtemp[x] 
            gul=gstring.upper()
            gtemp[x]=gul 
         gtempx.append(gtemp)      
         gtempx[0][0]=revmc
         fdict[key]= gtempx                                              # write the new elements to fdict as a nested list   
#
############################################################
#                                                            	
  for key in sorted(fdict.keys()):                                       # sort each entry in fdict (nested lists) in order of the revision
     temp=[]                                                             # no for each entry. 
     temp=fdict[key]
     if temp[0][0] == '':
       temp[0][0]='0' 
     temp.sort(key=itemgetter(0),reverse=True)
     fdict[key]=temp 
#     	
  for key in sorted(fdict.keys()):                                       # ascertain which fdict entrys have multiple versions
     temprev=[]                                                          # and add a "+" symbol to the revision number
     temprev=fdict[key]
     templ=len(temprev)
     if templ > 1:                                     
        for y in range(0,templ-1):                                       # spool through sub sets of the list   
            tiprev=temprev[y][0]
            tiprev="+"+tiprev
            temprev[y][0]=tiprev
        fdict[key]=temprev    
  return fdict  
#
####################################################################################################################################################################
# subroutine name: final_dictd
#
# purpose:         Merge the Global (extracted from the WMO txt file) and the current operational versions of BUFR table D
#
# input:           masterld      -   Dict of lists for the current operational Tabled (formatted as nested lists)
#                  mastergd      -   Dict of lists for version WMO Tabled
#
# output:          fdictd        -   Dict of lists.     
#
####################################################################################################################################################################
#
def final_dictd(masterld,mastergd):
  fdictd={}
  for key in masterld.keys():
     if key in mastergd:                    
       fdictd[key]=mastergd[key]
     else:
       fdictd[key]=masterld[key]
  #
  for key in mastergd.keys():
     if key not in fdictd:       
       fdictd[key]=mastergd[key]
  return fdictd   
#
####################################################################################################################################################################
# subroutine name: getdatb
#
# purpose:         read in the Global version of TableB (extracted from the WMO txt file) 
#
# input:           tablenb       -   TableB txt 
#
# output:          Tablebout     -   Dict of converted values.  
#                  version       -   WMO version of the tables   
#
####################################################################################################################################################################
#
def getDatb(tablenb): 
  vb=open(tablenb,'r') #   Open the txt file
  vname=tablenb.split("_")
  vf=vname[1] 
  vdot=".00"
  wversion=vf+vdot
  cline=0
  tablebout={}
  tlist=[]
  vb.next()
  for line in vb:
    tlist=[]
    stringtb=line.replace(";"," ")                    # new 20/01/16
    stringtb=stringtb.replace("\",\"",";")
    stringtb=stringtb.replace("\",,\"",";;")
    stringtb=stringtb.replace("\",,,\"",";;;")
    stringtb=stringtb.replace("\",,,,\"",";;;;")
    stringtb=stringtb.replace(",\"",";")
    stringtb=stringtb.replace("\",",";")
    stringtb=stringtb.replace("\"","")
    stringtb=stringtb.replace(","," ")
    stringbf=stringtb.split(";") 
    FXY=stringbf[3]
    desc=stringbf[4]
    units=stringbf[6]
    scale=stringbf[7]
    refs=stringbf[8]
    refs2=refs.strip()
    dwidth=stringbf[9]
    cunits=stringbf[10]
    cscale=stringbf[11]
    cwidth=stringbf[12]
    tlist.append(wversion)
    tlist.append(desc)
    tlist.append(units)
    tlist.append(scale)
    tlist.append(refs2)
    tlist.append(dwidth)
    tlist.append(cunits)
    tlist.append(cscale)
    tlist.append(cwidth)
    tablebout[FXY]=tlist
       
  return tablebout,wversion
#
####################################################################################################################################################################
# subroutine name: getdatd
#
# purpose:         read in the Global version of Tabled (extracted from the WMO txt file) 
#
# input:           tablend       -   Tabled txt 
#
# output:          Tabled        -   Dict of converted values.  
#                  version       -   WMO version of the tables   
#
####################################################################################################################################################################
#
def getDatd(Tablend):
  rowd=[]
  rowf=[]
  tabled={}
  tabledt={}
  count=0
  version=0
  cline=0
  vn=open(Tablend,'r') #
  vers=Tablend.split("_")
  version=vers[1]+"."+vers[2]+"00"
  next(vn)
  for line in vn:
    stringtf=line.replace(";"," ")                    # new 20/01/16
    stringtf=stringtf.replace("\",\"",";")
    stringtf=stringtf.replace("\",,\"",";;")
    stringtf=stringtf.replace("\",,,\"",";;;")
    stringtf=stringtf.replace(",\"",";")
    stringtf=stringtf.replace("\",",";")
    stringtf=stringtf.replace("\"","")
    stringtf=stringtf.replace(","," ")
    stringt=stringtf.split(";")
 #   print "stringt ",stringt,"\n"
    keystr = stringt[3]
    keyst=keystr.lstrip(" ")
    keyst2=keyst.lstrip(" ")
#    keyst2=keyst.rstrip(" ")
    keyst=keyst2     
    ints=len(keystr)                                                                                       # sequence title
    comp = stringt[6]
    if keyst not in tabled.keys(): 
       seqt = stringt[4]           
       rowd=[] 
       rowd.insert(0,seqt)
       rowd.append(comp)
       tabled[keyst]=rowd
    else :
       rowd=tabled[keyst]
       rowd.append(comp)
       tabled[keyst]=rowd 
  for key in tabled.keys():
      templ=tabled[key]
      count=len(templ[1:])
      templ.insert(1,count)
      tabled[key]=templ     
  return tabled,version
#
####################################################################################################################################################################
# subroutine name: output_b
#
# purpose:         output BUFR TableB  
#
# input:           f_dict     -  dict of lists, containg all versions of the descriptor
#                  O_ver      - Version no of the WMO xml data
#                  N_Ver      - UKMO Ver no of the operational BUFR tableB 
#
# output:          BUFR Table B - BUFR_TableB_YY-MM-DD_HH:MM:SS.txt     
#                    
#
####################################################################################################################################################################
#
def output_b(f_dict,UKMO_ver,WMO_ver,build_dir):
   dummy="xxxxxxxxx               0   0  O\n"
   pwd=os.getcwd()
   i=datetime.now()
   sxt=i.strftime('%d %b %Y')
   sxtb="("+sxt+")" 
   xt=i.strftime('%Y/%m/%d %H:%M:%S') 
   xt=i.strftime('%d/%m/%Y %H:%M:%S') 
   xtfa=xt[0:6]
   xtfb=xt[8:]
   xtf=xtfa+xtfb 
   xp=i.strftime('%Y-%m-%d_%H:%M:%S')  
   filename=build_dir+"/output/BUFR_TableB_"+xp+".txt"
   bf=open(filename,'w') 
   mlen=0
   maxl=0  
   n=0

   F_WMO=float(WMO_ver) 
   I_WMO=int(F_WMO)
   S_WMO=str(I_WMO)
   
   F_UKMO=float(UKMO_ver)
   F_UKMO=F_UKMO+1
   F_UKMO=int(F_UKMO)
   S_UKMO=str(F_UKMO)
   
   str1="WMO Version "
   fstr1=str1+S_WMO+".0.0"  
   fstr2=str1+S_WMO  

   xt=xt
   O_xter=str(xt)
   
   for key in sorted(f_dict.keys()):
      y=len(f_dict[key])
      
      if y > n:                     # n - establish the max no of elements in a list
        n=y
   vstring1='{message:{fill}{align}{width}}'.format(message=fstr1,fill=' ',align='>',width=21)
   vstring2='{message:{fill}{align}{width}}'.format(message=sxtb,fill=' ',align='>',width=17)
   vstring3='{message:{fill}{align}{width}}'.format(message=fstr2,fill=' ',align='>',width=38)	
     
   bf.write(('%s')%(vstring1))
   bf.write(('%s')%(vstring2))
   bf.write(('%s\n')%(vstring3))
   for x in range(0,n):	
     for key in sorted(f_dict.keys()):
        lenf=len(f_dict[key])
	if x < lenf:
	  string1=f_dict[key][x][1]
	  string1=string1.strip()

          lenit=len(f_dict[key][x])
	  if (len(string1)) > 60:
	     string1=string1[0:59] 
          key.strip()                                                                           
          fkey='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=7)	                              # F X Y identifier      
          bf.write(('%s')%(fkey)) 
          #   
          string1.upper() 
          string1.strip()
          string1.lstrip()                                                                       
          fstring1='{message:{fill}{align}{width}}'.format(message=string1,fill=' ',align='<',width=65)                      # F X Y Description 
          bf.write(('%s')%(fstring1)) 
          #                             
	  string3=f_dict[key][x][0] 
	  string3=string3.strip()  
          if string3.find("+") == -1 and len(string3) < 6:
              string3a = string3 + "0"
              string3 = string3a
          elif string3.find("+") > -1 and len(string3) < 7:
              string3a = string3 + "0"
              string3 = string3a
          fstring3='{message:{fill}{align}{width}}'.format(message=string3,fill=' ',align='>',width=8)	                      # version number  
          bf.write(('%s\n')%(fstring3))
          #
          stringu=f_dict[key][x][2]
          stringu=stringu.lstrip() 
          if len(stringu) > 26:
             stringu2=stringu[0:25]
             stringu=stringu2
          if (stringu.find("CODE TABLE") >= 0 or stringu.find("code table") >= 0) and stringu.find("COMMON CODE TABLE") == -1:
              stringu =  "CODE TABLE"                                                                     
          stringu=" "+stringu
          fstringu='{message:{fill}{align}{width}}'.format(message=stringu,fill=' ',align='<',width=23)	                      # BUFR unit    
          bf.write(('%s')%(fstringu))                                                                         
          #
          stringsc= f_dict[key][x][3]                                                                      
          fstringsc='{message:{fill}{align}{width}}'.format(message=stringsc,fill=' ',align='>',width=6)                     # BUFR scale
          bf.write(('%s')%(fstringsc))                                                               
          # 
                
	  string4=f_dict[key][x][4]
          string4.strip()
          fstringrf='{message:{fill}{align}{width}}'.format(message=string4,fill=' ',align='>',width=11)                      # BUFR reference value
          bf.write(('%s')%(fstringrf))
          #           
	  string5=f_dict[key][x][5]
          fstringw='{message:{fill}{align}{width}}'.format(message=string5,fill=' ',align='>',width=3)                       # BUFR data width  
          #
          if lenit > 6:
             # 
             bf.write(('%s')%(fstringw))
             # 
             stringcrex1=f_dict[key][x][6]
             stringcrex1=" "+stringcrex1
             fstringu='{message:{fill}{align}{width}}'.format(message=stringcrex1,fill=' ',align='<',width=26)                 # CREX unit
             bf.write(('%s')%(fstringu))
             #
             stringcrex2=f_dict[key][x][7]
             fstrings='{message:{fill}{align}{width}}'.format(message=stringcrex2,fill=' ',align='>',width=3)                 # CREX Scale
             bf.write(('%s')%(fstrings))
             #
             stringcrex3=f_dict[key][x][8]
             fstringw='{message:{fill}{align}{width}}'.format(message=stringcrex3,fill=' ',align='>',width=3)                 # CREX data width      
             bf.write(('%s')%(fstringw))
             stringcrex4=" O"
             fstringo='{message:{fill}{align}{width}}'.format(message=stringcrex4,fill=' ',align='>',width=2)                 # CREX spacer char O
             bf.write(('%s\n')%(fstringo))
          else:
             bf.write(('%s\n')%(fstringw))                                                                                    # BUFR data width (assuming no CREX data available)
             #
   print "===================================================================================================================================\n"
   print "\n"
   print " BUFR TableB output - ",filename,"\n"
   print "\n"
   print "\n"
	  
#
####################################################################################################################################################################
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
####################################################################################################################################################################
#
def output_d(fdictd,revn,wmo_rev,build_dir):
 i=datetime.now()
 sxt=i.strftime('%d %b %Y')
 sxtb="("+sxt+")" 
 xt=i.strftime('%Y-%m-%d_%H:%M:%S')
 pwd=os.getcwd()   
 fdict_out=build_dir+"/output/BUFR_TableD_"+xt+".txt"
 rp=revn.find(".")
 renip=revn[:rp]
 reni=int(renip)
 reni=reni+1
 renc=str(reni)


 str1="WMO Version "
 fstr1=str1+renc+".0.0"  

 fdo=open(fdict_out,'w') #
 vstring1='{message:{fill}{align}{width}}'.format(message=fstr1,fill=' ',align='>',width=22)
 vstring2='{message:{fill}{align}{width}}'.format(message=sxtb,fill=' ',align='>',width=16)
 fdo.write(('%s') % (vstring1)) 
 fdo.write(('%s\n') % (vstring2))
 fdo.write(('%s\n') % (" "))
 #
 for key in sorted(fdictd):
   
   templ=[]                                
   templ=fdictd[key][2:]                                                                        # Assign D sequence to temp list. Excluding the title and count                      
   rl=len(templ)                                                                               # length of the list containing the D sequence                                                                       
   desct=fdictd[key][0]
   desctsf="          "+fdictd[key][0]                                                         # sequence title 
   descts=desct.strip(" ")
   if len(descts) == 0:
      desct="NIL"
   desc_c=fdictd[key][1] 
   if isinstance(desc_c,str):
     desc_ci=int(desc_c)
     desc_c=desc_ci                                                                             # sequence count
   if desc_c > 99:
      kwidth=6
      cwidth=4
#   elif desc_c > 10 and desc_c < 99:
   elif desc_c >= 10 and desc_c <= 99:
      kwidth=7 
      cwidth=3
   elif desc_c < 10:
      kwidth=8 
      cwidth=2
          
   Dkey='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=kwidth)            # format output for D sequence key
   fcount='{message:{fill}{align}{width}}'.format(message=desc_c,fill=' ',align='<',width=cwidth) 	# format output for sequence count
   fdesct='{message:{fill}{align}{width}}'.format(message=desctsf,fill=' ',align='<',width=70)            # format output for descriptor title   
   if desct != "NIL" : 
      fdo.write(('%s\n')%(fdesct))                                                              # output sequence title (if present) followed by new line     
   fdo.write(('%s')%(Dkey))                                                                     # output D sequence key 
   fdo.write(('%s')%(fcount))                                                                   # output D sequence count    
   for x in range(0,rl):
       descd_out=templ[x]
       Descd='{message:{fill}{align}{width}}'.format(message=descd_out,fill=' ',align='<',width=7)  
       xrem=x%10
       if xrem == 0 and x != 0:
         fdo.write(('%s\n')%(" "))
         fdo.write(('%s')%(Descd))
       else:
         fdo.write(('%s')%(Descd))
   fdo.write(('%s\n')%("  "))
   fdo.write(('%s\n')%("  "))
 fdo.close() 
 print "===================================================================================================================================\n"
 print "\n"
 print " BUFR TableD output - ",fdict_out,"\n"
 print "\n"	  
####################################################################################################################################################################
# subroutine name: comp_versions
#
# purpose:         compare current and new versions of BUFR TableB or TableD D.
#                  Output revision information of descriptors or sequences deprecated and/or added
#
# input:           nvers      - new version of the table 
#                  overs      - old version of the table 
#                  table      - B or D indicator 
#                  wmo_rev    - wmo version no
#                  loc_rev    - local (metDB) version no
#                  bdir       - Build/output directory
#
# output:          Revision information  - TableB_Revision_Information_YY-MM-DD_HH:MM:SS.txt
#                                          TableD_Revision_Information_YY-MM-DD_HH:MM:SS.txt     
#                    
#
####################################################################################################################################################################
# 
def comp_versions(nvers,overs,table,wmo_rev,loc_rev,build_dir):
  ic=datetime.now()
  xc=ic.strftime('%Y-%m-%d_%H:%M:%S')
  pwd=os.getcwd() 
  c_dict={} 
  m_dict={} 
  fctemp="test_out.txt" 
  fcp=open(fctemp,'w') 
  if table == "B": 
    fc_out=build_dir+"/output/TableB_Revision_Information"+xc+".txt"
    fco=open(fc_out,'w') #   
    for key in nvers.keys():               # where entries are in both dict / tables
      if key in overs:        
        lenov=len(overs[key])
        tlstb=[]  
        for z in range(0,lenov):
          tlsta=[]
          tlsta=overs[key][z][3:6]  
          tlst1a=[x.strip(' ') for x in tlsta]   
          tlstb.append(tlst1a)  
        flist=set(tuple(x) for x in tlstb)     
                                                       # produces set of non matching tuples 
        flist_lt=list(flist)                           # convert back to list of non matching tuples 
        flen=len(flist_lt)                             # flen corresponds to the number of tuple lists within the list                   
        if flen > 1:
          c_dict[key]=overs[key]
      else:  # elements in the new version that are not in the old version - Note will ignore local entrys 
        m_dict[key]=nvers[key]	
#
    spacer=" "
    Bspt='{message:{fill}{align}{width}}'.format(message=spacer,fill=' ',align='<',width=65)
    vspacer="|"
    Bvspac='{message:{fill}{align}{width}}'.format(message=vspacer,fill=' ',align='<',width=4)
    bdiv="+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    Btdiv='{message:{fill}{align}{width}}'.format(message=bdiv,fill=' ',align='<',width=65)
    title="Alterations to existing descriptors "
    Btitlet='{message:{fill}{align}{width}}'.format(message=title,fill=' ',align='<',width=65) 
    rev="Revision"
    Brevt='{message:{fill}{align}{width}}'.format(message=rev,fill=' ',align='<',width=17) 
    FXY="FXY"
    BFXYt='{message:{fill}{align}{width}}'.format(message=FXY,fill=' ',align='<',width=16) 
    unit="Unit"
    Bunitt='{message:{fill}{align}{width}}'.format(message=unit,fill=' ',align='<',width=18)   
    scale="Scale"
    Bscalet='{message:{fill}{align}{width}}'.format(message=scale,fill=' ',align='<',width=13) 
    ref="Reference"
    Breft='{message:{fill}{align}{width}}'.format(message=ref,fill=' ',align='<',width=14)
    dw="Data Width"
    Bdw='{message:{fill}{align}{width}}'.format(message=dw,fill=' ',align='<',width=20)   
    desc="Description" 
    Bdesct='{message:{fill}{align}{width}}'.format(message=desc,fill=' ',align='<',width=33)  
    endt="Comparison run Date / Time -"+xc
    Bendt='{message:{fill}{align}{width}}'.format(message=endt,fill=' ',align='<',width=30)  
    output="Descriptors introduced in WMO version "+wmo_rev+" of BUFR Table B" 
    Bintro='{message:{fill}{align}{width}}'.format(message=output,fill=' ',align='<',width=40)
    Bspac='{message:{fill}{align}{width}}'.format(message=" ",fill=' ',align='<',width=11) 
    flist=[11,30,16,10,10,10]
#
    if any(c_dict):
       fco.write(('%s\n')%(Btitlet))
       fco.write(('%s\n')%(Bspt)) 
       fco.write(('%s')%(BFXYt))
       fco.write(('%s')%(Brevt))
       fco.write(('%s')%(Bdesct))
       fco.write(('%s')%(Bunitt))
       fco.write(('%s')%(Bscalet))
       fco.write(('%s')%(Breft))
       fco.write(('%s\n')%(Bdw))  
      
       for key in sorted(c_dict):  
          CFXY='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=11)  	
          templ= c_dict[key]
          fco.write(('%s\n')%(" "))  
          fco.write(('%s')%(CFXY)) 
          fco.write(('%s')%(Bvspac))       
          tl= len(templ)  
          for x in range(0,tl):                   # spool through sub lists
             plist=templ[x] 
             for y in range(0,6):                 # spool through lists contents
                fam=flist[y] 
                op_val=plist[y]
                if y == 1:
                  if len(op_val) > 24:
                    op_val=op_val[0:23]   
                if y == 0:
                  if op_val.find("+") > -1:
                    opval2=op_val[1:]
                    op_val=opval2 
                if y == 5:   
                    Bopv='{message:{fill}{align}{width}}'.format(message=op_val,fill=' ',align='<',width=fam) 
                    fco.write(('%s\n')%(Bopv))
                else:
                    if x > 0 and y == 0:
                       Bopv='{message:{fill}{align}{width}}'.format(message=op_val,fill=' ',align='<',width=fam) 
                       fco.write(('%s')%(Bspac)) 
                       fco.write(('%s')%(Bvspac))
                       fco.write(('%s')%(Bopv)) 
                       fco.write(('%s')%(Bvspac))
                    else:    
                       Bopv='{message:{fill}{align}{width}}'.format(message=op_val,fill=' ',align='<',width=fam) 
                       fco.write(('%s')%(Bopv))
                       fco.write(('%s')%(Bvspac)) 
    fco.write(('%s\n')%(Bspac))
    fco.write(('%s')%(Bintro))
    fco.write(('%s\n')%(Bspt))
    

    if any(m_dict):
      fco.write(('%s\n')%(Bspt))
      fco.write(('%s')%(BFXYt))
      fco.write(('%s')%(Brevt))
      fco.write(('%s')%(Bdesct))
      fco.write(('%s')%(Bunitt))
      fco.write(('%s')%(Bscalet))
      fco.write(('%s')%(Breft))
      fco.write(('%s\n')%(Bdw))  
      for key in sorted(m_dict):	
         B2key='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=13)
         fco.write(('%s')%(B2key))
         fco.write(('%s')%(Bvspac))
         for y in range(0,6):
            fam=flist[y]   
            op_val2=m_dict[key][y]
            if y == 1:
              if len(op_val2) > 24:
                 op_val2=op_val2[0:23]   
            B2opv='{message:{fill}{align}{width}}'.format(message=op_val2,fill=' ',align='<',width=fam) 
            if y == 5:
              fco.write(('%s\n')%(B2opv))
            else: 
               fco.write(('%s')%(B2opv))
               fco.write(('%s')%(Bvspac))

    print "===================================================================================================================================\n"
    print "\n"
    print "Table B Revision output -",fc_out,"\n"
    print "\n"
    print "===================================================================================================================================\n" 
# applies to TableD  
#
###########################################################################################################################################################################  
#
  elif table == "D":
    ic=datetime.now()
    xc=ic.strftime('%Y-%m-%d_%H:%M:%S')
    pwd=os.getcwd() 
    c_dict={} 
    m_dict={} 
    fc_out=build_dir+"/output/TableD_Comparison"+xc+".txt" 
    fco=open(fc_out,'w') #
    seq_dep_new={}                                                    # dict - sequences removed in the new version  
    seq_add_new={}                                                    # dict - sequences added in the new version           
    dep_old={}                                                        # dict - descriptors deprecated from existing sequences
    add_new={}                                                        # dict - descriptors added to existing sequences
    list_old=[]
    list_new=[]
    flistn=[]
    flisto=[]                                                         # is the new sequence in the old version of the tables, ie has it been introduced with version XX
    for key in nvers.keys():
       flistn=[]
       flisto=[] 
       key2=int(key[3:])
       intl=len(key)      
       if key in overs.keys() and key2 < 192:                            # if yes has the entry been altered in the new edition
          list_new=overs[key][2:]
          list_old=nvers[key][2:]                                                           
          ast=set(list_new)                                            
          bst=set(list_old)                                         
          edep=bst.difference(ast)                                      # existing sequences from which descriptors have been deprecated (removed)
          nadd=ast.difference(bst)                                      # existing sequences to which new (additional ) descriptors have been added 
          intnew=list(nadd)
          intold=list(edep) 
          if len(intnew) > 0:                                                  # list length greater than zero
             add_new[key]=intnew
          if len(intold) > 0:                                                 # list length greater than zero
             dep_old[key]=intold
       elif key not in overs.keys() and key2 < 192:   
          list_new=[]                                                          # sequences added in the new edition
          list_new=nvers[key]
          seq_add_new[key]=list_new                                        
    for key in overs:                                                            # sequences removed completely from the new version
       key2=int(key[3:])  
       if key not in nvers and key2 < 192:
          list_old=[] 
          list_old=overs[key]
          seq_dep_new[key]=list_old            
    blankm="+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    blanki="-----------------------------------------------------------------------------------------------------"
    titlem="  FXY   |                   Descriptors / Sequences     "
    titlem2="       |  "
    spacer  = "             Descriptors added to existing D sequences in the new version of Table D                       "    # (test1) 
    spacer1 = "                D Sequences added in the new version of Table D                "                                   # (test2)
    spacer2 = "             Descriptors deprecated from D sequences in the new version of Table D              "               # (test3)
    spacer3 = "             D Sequences deprecated from the new version of Table D                               "             # (test4)
    
    nill="              No Entries  "
    bout= '{message:{fill}{align}{width}}'.format(message=blankm,fill=' ',align='<',width=40)  
    boutd= '{message:{fill}{align}{width}}'.format(message=blanki,fill=' ',align='<',width=40)
    bout0= '{message:{fill}{align}{width}}'.format(message=titlem,fill=' ',align='<',width=40)
    fdec='{message:{fill}{align}{width}}'.format(message=spacer,fill=' ',align='<',width=35)  
    fdec1='{message:{fill}{align}{width}}'.format(message=spacer1,fill=' ',align='<',width=35) 
    fdec2='{message:{fill}{align}{width}}'.format(message=spacer2,fill=' ',align='<',width=35)  
    fdec3='{message:{fill}{align}{width}}'.format(message=spacer3,fill=' ',align='<',width=35) 
    null2='{message:{fill}{align}{width}}'.format(message=nill,fill=' ',align='<',width=35)
###################################################################################################
    if any(seq_dep_new):                                                # output details of sequences removed in the new version  
       fco.write(('%s\n')%(bout))
       fco.write(('%s\n')%("  "))
       fco.write(('%s\n')%(fdec3))
       fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout0))
       fco.write(('%s\n')%(boutd))
       for key in seq_dep_new.keys():
          olist=[]
          olist=seq_dep_new[key]
          title=seq_dep_new[key][0]
          leno=len(olist)
          Dkey='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=8)
          sp3='{message:{fill}{align}{width}}'.format(message='|',fill=' ',align='>',width=9)  
          sp2='{message:{fill}{align}{width}}'.format(message='| ',fill=' ',align='<',width=1)  
          titleo='{message:{fill}{align}{width}}'.format(message=title,fill=' ',align='<',width=22)  
          fco.write(('%s')%(sp3))
          fco.write(('%s\n')%(titleo))
          fco.write(('%s')%(Dkey))
          fco.write(('%s')%(sp2))
          
          for x in range(2,leno):
              descb_out=olist[x]
              Descb='{message:{fill}{align}{width}}'.format(message=descb_out,fill=' ',align='<',width=8)  
              sp1='{message:{fill}{align}{width}}'.format(message=' ',fill=' ',align='<',width=8)
              xrem=x%10
              if xrem == 0 and x != 0:
                fco.write(('%s\n')%(""))
                fco.write(('%s')%(sp1))
                fco.write(('%s')%(sp2))
                fco.write(('%s')%(Descb))
              else:
                fco.write(('%s')%(Descb))
          fco.write(('%s\n')%(""))
          fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout))
#####################################################################################################################################################################
    if any(seq_add_new):                                               # output details of sequences added in the new version                 
       fco.write(('%s\n')%("  "))
       fco.write(('%s\n')%(fdec1))
       fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout0))
       fco.write(('%s\n')%(boutd))
       for key in seq_add_new.keys():
          olist=[]
          otlist=seq_add_new[key][1:]
          title=seq_add_new[key][0]
          lenot=len(otlist)
          Dkey='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=8)  
          sp2='{message:{fill}{align}{width}}'.format(message='| ',fill=' ',align='<',width=1)  
          if title == "NIL":
             title= " "
          Dt= '{message:{fill}{align}{width}}'.format(message=title,fill=' ',align='<',width=60)
          fco.write(('%s')%(Dkey))
          fco.write(('%s')%(sp2))
          for x in range(2,lenot):  # changed here
              desct_out=otlist[x]
              Descbt='{message:{fill}{align}{width}}'.format(message=desct_out,fill=' ',align='<',width=8)
              sp1='{message:{fill}{align}{width}}'.format(message=' ',fill=' ',align='<',width=8)     
              xrem=x%10
              if xrem == 0 and x != 0:
                 fco.write(('%s\n')%(" "))
                 fco.write(('%s')%(sp1))
                 fco.write(('%s')%(sp2))
                 fco.write(('%s')%(Descbt))
              else:
                 fco.write(('%s')%(Descbt))
          fco.write(('%s\n')%("  "))
          fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout))
####################################################################################################################################################################
    if any(dep_old):                                                    # output details of descriptors deprecated from sequences in the new version
       fco.write(('%s\n')%("  "))
       fco.write(('%s\n')%(fdec2))
       fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout0))
       fco.write(('%s\n')%(boutd))
       for key in dep_old.keys():
          nlist=[]
          nlist= dep_old[key]
          lenn=len(nlist)
          Dkeyo='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=8) 
          sp2='{message:{fill}{align}{width}}'.format(message='| ',fill=' ',align='<',width=1)    
          fco.write(('%s')%(Dkeyo))
          fco.write(('%s')%(sp2))
          for x in range(0,lenn):
              descb_out=nlist[x]
              Descot='{message:{fill}{align}{width}}'.format(message=descb_out,fill=' ',align='<',width=8) 
              sp1='{message:{fill}{align}{width}}'.format(message=' ',fill=' ',align='<',width=8)      
              xrem=x%10
              if xrem == 0 and x != 0:
                 fco.write(('%s\n')%(" "))
                 fco.write(('%s')%(sp1)) 
                 fco.write(('%s')%(sp2))         
                 fco.write(('%s')%(Descot))
              else:
                 fco.write(('%s')%(Descot))
          fco.write(('%s\n')%("  "))
          fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout))
####################################################################################################################################################################
    if any(add_new):     
       fco.write(('%s\n')%("  "))
       fco.write(('%s\n')%(fdec))
       fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout0))
       fco.write(('%s\n')%(boutd))
       for key in add_new.keys():
          ntlist=[]
          ntlist=add_new[key]
          title=add_new[key][0]
          leno=len(ntlist)
          Dkey='{message:{fill}{align}{width}}'.format(message=key,fill=' ',align='<',width=8)  
          if title == "NIL":
             title= " "
          Dt='{message:{fill}{align}{width}}'.format(message=title,fill=' ',align='<',width=60)
          fco.write(('%s')%(Dkey))
          fco.write(('%s')%(sp2))
          for x in range(0,leno):
              desct_out=ntlist[x] 
              Descott='{message:{fill}{align}{width}}'.format(message=desct_out,fill=' ',align='<',width=8) 
              xremt=x%10
              if xremt == 0 and x != 0:
                 fco.write(('%s\n')%(" "))
                 fco.write(('%s')%(sp1)) 
                 fco.write(('%s')%(sp2))         
                 fco.write(('%s')%(Descott))
              else:
                 fco.write(('%s')%(Descott)) 
          fco.write(('%s\n')%("  "))
          fco.write(('%s\n')%(boutd))
       fco.write(('%s\n')%(bout)) 
####################################################################################################################################################################
    print "***************\n"
    print "\n"		  
    print " BUFR Table D Comparison output -",fc_out,"\n"
    print "\n"
    print "***************\n"		                        
####################################################################################################################################################################
# subroutine name: read_in_tableb
#
# purpose:         Read in the current (operational) tableB to a dict.       
#
# input:           Tablename  -  BUFR TableB filename (.txt)  
#
# output:          Master     - Dict of lists of the form -
#
#                 dict[010114][[rev(1),Descr,unit,scale,ref val,data width],[rev(2),Descr,unit,scale,ref val,data width]]  
#                  
#                 rev_ukmo - local table revision information
#                 
#                    
#
####################################################################################################################################################################
def read_in_tableb(tablename):
 rev="WMO Version"
 second=0
 drow=[]
 master={}
 local={}
 with open (tablename,'r') as fb:                               
    for line in fb:
      if rev not in line:                                       
        test=line[0:6]
        if test.isdigit() and second == 0:                     
	  ident=str(line[0:6]) 
	  ident=ident.strip(' \t\n\r')  
	  gident=int(ident[0:3])
	  lident=int(ident[3:6])                              
	  desc=line[7:61]                                       
	  desc=desc.strip(' \t\n\r')
          metrev=line[74:]
	  metrev2=metrev.replace("+","")
	  metrev=metrev2
	  metrev=metrev.strip(' \t\n\r')
	  second=1
        elif second == 1: 
	  drow=[]     
          unit=line[1:13] 
	  unit=unit.strip(' \t\n\r')                                   
	  scale=line[26:29]                                    
	  scale=scale.strip(' \t\n\r')
	  ref=line[29:40]
	  ref=ref.strip(' \t\n\r')
	  dw=line[40:43]
	  dw=dw.strip(' \t\n\r') 
          cunit=line[44:67]
	  cunit=cunit.strip(' \t\n\r')
	  cscale=line[70:73]    
	  cscale=cscale.strip(' \t\n\r')
#          cdw=line[74:76]
          cdw=line[73:76] 
          cdw=cdw.strip(' \t\n\r') 
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
	     drowt=master[ident]
	     drowt.append(drow)
	     xl=len(master[ident])                              
	     if xl > 1:                                         
	       drowt.sort(key=itemgetter(0))                     
	       master[ident]=drowt              # apend to existing entry in master dict                                                       
          else:
	     drowt=[]
	     drowt.append(drow)
	     drow=list(drowt)
	     master[ident]=drow                 # add new entry to master dict
          second=0     						 
      else:
         rev_line=line[0:] 
	 point=rev_line.find(rev)
	 rev_ukmo=rev_line[point+12:point+16]
 for key in sorted(master.keys()):
     tempm=[]
     tempm=master[key]
     tempm.sort(key=itemgetter(0),reverse=True)
     master[key]=tempm 	 
 by=open('testsd.txt','a') 
 for key in master.keys():
     by.write(('%s') % ('read_in_tableb :key => '))
     by.write(('%s') % (key))
     by.write(('%s') % (' => ')) 
     by.write(('%s\n') % (master[key])) 
 by.close()     					  
 return master,rev_ukmo
#  note rev_ukmo - UKMO revision no
####################################################################################################################################################################
# subroutine name: read_in_tabled
#
# purpose:         Read in the current (operational) BUFR TableD to a dict.       
#
# input:           Tablename  -  BUFR TableD filename (.txt)  
#
# output:          dseq      - Dict of lists 
#                   
#
####################################################################################################################################################################
#
def read_in_tableD(tableD):
 dseq={}
 # rev=False
 rev2=""
 dline=[]
 n=0
 nseq=0
 fin=open(tableD,'r') #  
 for line in fin:                                                        # check for first line of file and extract version no
     bc=line.rstrip(" ")
     bc2=bc.lstrip(" ")
     bc3=bc2.strip("\n")
     bc4=bc3.strip("\r")
     lbc=len(bc4)
     if line.find('WMO Version') != -1 :
        rev2=line[15:19]
	rev2.strip()                                          
     else:
        if lbc == 0:                                                    # check for blank line, this may indicate the start of a new sequence
           nseq=0                                                       # re set switch to indicate start of a new sequence
           title=""
        elif nseq == 0 and lbc > 0:                                     # switch indicates the start of a new sequence
          if bool(re.search('[a-zA-Z]',line)):                          # check for sequence title if found insert at index 0 in the list
             title=""
             title=line
             title.lstrip()
             title.rstrip()
             titlef=title.replace("(","")
             titlef2=titlef.replace(")","")
             titlef2=re.sub( '\s+', ' ', titlef2 ).strip()
             title=titlef2                                    
          else:                                                         # numeric string found, assume 1st line of sequence
             temp_l=re.sub( '\s+', ' ', line ).strip()                  # replace multiple whitespace in the line with single whitespace
             templ=temp_l.split(" ")                                    # split string on single whitespace
             if len(templ[0]) > 6:
                key=templ[0][0:6]
                count=templ[0][6:]
             else:
                key=templ[0]
                count=templ[1] 
             dline=templ[2:] 
             dline.insert(0,title)
             dline.insert(1,count)
             nseq=nseq+1
             dseq[key]= dline
        elif lbc > 0 and nseq > 0:                                                 # switch indicates the continuation of an existing sequence
             temp_l=re.sub( '\s+', ' ', line ).strip()                 # replace multiple whitespace in the line with single whitespace
             templ=temp_l.split(" ")                                   # split string on single whitespace
             dline=dline+templ 
             dseq[key]= dline
 return dseq,rev2
    	 	
####################################################################################################################################################################
# subroutine name: read_oldfig
#
# purpose:         Read in the current (operational) BUFR Codefigs to a dict.       
#
# input:           Tablename  -  BUFR Codefigs (.txt)  
#
# output:          fin_dict      - Dict of Codefigs
#                  rev_no        - Revision No of the current version of codefigs 
#                   
#
####################################################################################################################################################################
def read_oldfig(file1):
 fin_dict={}
 clist=[]
 tlist=[]
 with open (file1,'r') as fin:
   for line in fin:
     if line.find("$Revision") >= 0:                # 1st line contains revision no
       rev_no=int(line[12:14])
       line=fin.next()                             # skip next 2 lines to start of code table
       line=fin.next() 
     line_test=line.strip()
     if line_test:                                # if line was not blank  
        if line.find("!") >=0:
           ex=line.find("!")
           line=line[:ex-1]
        line=line.rstrip() 
        linea=line.split() 
        el=len(linea[0])
        elf=len(linea)
        if el == 6:                               # assume sequence (key) & title (will check for 6 digit desc) 
           tlist=[]
           clist=[]
           key=linea[0]
           ct_start=line.find(linea[1])
           ct_title=line[ct_start:]
           ct_title=ct_title.rstrip()
           tlist.append(ct_title)
           clist.append(tlist)
        else:
           if elf == 1:
             linea.append("-")
           tlist=[]
           entry_no=linea[0]    
           ent_start=line.find(linea[1])
           ent_det=line[ent_start:]
           tlist.append(entry_no)
           tlist.append(ent_det)
           clist.append(tlist)
     else:
        fin_dict[key]=clist
 fin.close()    
 return(fin_dict,rev_no)
#
####################################################################################################################################################################
# subroutine name: read_newfig
#
# purpose:         Read in the WMO version of BUFR Codefigs to a dict.       
#
# input:           Tablename  - WMO Codefigs (.txt)  
#
# output:          fin_dict      - Dict of Codefigs
#                  rev_no        - Revision No of the current version of codefigs 
#                   
#
####################################################################################################################################################################
#
def read_newfig(file2):
# dict structure => ct_dict[01003]={[[table Title] [table entry no,table entry],[table entry no,table entry],[table entry no,table entry]]}
#                   where there are full entries are available for the code table. Alternativly the dict will be of the structure 
#                   ct_dict[01003]={[[table title] [NIL]]}
#                   This may signify that a reference has been made to the common code tables.
#               
 ct_dict={}
 ct_ent=[]
 ct_holder=[]
 with open (file2,'r') as fin2:
   bline=fin2.next()
   for ct in fin2:  
       ct=ct.split(",")
       test_list=[x for x in ct if x]
       if len(test_list) > 4:  
          tab_no=ct[1]                             # Dict key
          tab_title=ct[2]
          tab_ref=ct[3]
          tab_ent=ct[4]
          tab_ref2=tab_ref.strip("\"") 
          tab_title2=tab_title.strip("\"") 
          tab_ent2=tab_ent.strip("\"") 
          tab_no2=tab_no.strip("\"")  
          if tab_no2 in ct_dict:
             ct_ent=[] 
             ct_ent.append(tab_ref2)
             ct_ent.append(tab_ent2)
             ct_holder.append(ct_ent)
             ct_dict[tab_no2]=ct_holder
          else:
             ct_ent=[]
             ct_holder=[] 
             ct_ent.append(tab_title2) 
             ct_holder.insert(0,ct_ent)
             ct_ent=[]
             ct_ent.append(tab_ref2)
             ct_ent.append(tab_ent2)
             ct_holder.append(ct_ent)
             ct_dict[tab_no2]=ct_holder
       else:                                         # where entries are blank refs to the common code tables may be needed
             tab_no=ct[1]                             # Dict key
             tab_title=ct[2]
             tab_no2=tab_no.strip("\"")  
             tab_title2=tab_title.strip("\"")   
             if tab_no2 not in ct_dict:
                ct_ent=[]
                ct_holder=[]
                tab_title3=tab_title2+" Refer to common code table" 
                ct_ent.append(tab_title3)
                ct_holder.append(ct_ent)
                ct_dict[tab_no2]=ct_holder 
 fin2.close() 
 return(ct_dict) 
#
####################################################################################################################################################################
# subroutine name: mdict
#
# purpose:         merge WMO and MetDB versions of the BUFR Codefig tables
#
# input:           masterld     -  local version of the  codefig tables
#                  mastergd     -  WMO Version (New) version of the code tables
#
# output:          fdictd       -  Merged data in a dictionary  
#                    
#
#################################################################################################################################################################### 
#
def mdict(masterld,mastergd):
  fdictd={}
  for key in masterld.keys():
     if key in mastergd:                    
       fdictd[key]=mastergd[key]
     else:
       fdictd[key]=masterld[key]
  #
  for key in mastergd.keys():
     if key not in fdictd:       
       fdictd[key]=mastergd[key]
  return fdictd  
#
####################################################################################################################################################################
# subroutine name: out_codefig
#
# purpose:         output BUFR Codefig tables
#
# input:           mfdict     -  Merged codefig tables
#
# output:          BUFR CodeFig Table - CodeFig_YY-MM-DD_HH:MM:SS.txt     
#                    
#
####################################################################################################################################################################
#
def out_codefig(mfdictd,rno,build_dir):
 i=datetime.now()
 xt=i.strftime('%Y-%m-%d_%H:%M:%S')
 xp=i.strftime('%d/%m/%Y %H:%M:%S')   
 pwd=os.getcwd() 
 fdict_out=build_dir+"/output/CodeFig_"+xt+".txt"
 
 fdo=open(fdict_out,'w') #
 #
 revno=rno+1
 revno2=str(revno) 
 tline="  $Revision: "+revno2+"$Date:  "+xp+"$"
 tlineo='{message:{fill}{align}{width}}'.format(message=tline,fill=' ',align='<',width=46) 
 fdo.write(('%s\n')%(tlineo))
 fdo.write(('%s\n')%("  ")) 
 for key in sorted(mfdictd):
   title=mfdictd[key][0][0]  
   key2="  "+key
   fkey='{message:{fill}{align}{width}}'.format(message=key2,fill=' ',align='>',width=8) 
   title=" "+title 
   title=title.upper()        
   ftitle='{message:{fill}{align}{width}}'.format(message=title,fill=' ',align='<',width=40)    
   fdo.write(('%s')%(fkey))                                                                      
   fdo.write(('%s\n')%(ftitle))                                                        
   arr=mfdictd[key][1:]                                                                         # exclude first entry in length calc
   lent=len(arr) 
   for y in range(0,lent):
       cno=arr[y][0]
       entry=arr[y][1]
       entry=entry.upper()
       fcno='{message:{fill}{align}{width}}'.format(message=cno,fill=' ',align='<',width=4)             # entry no
       fentry='{message:{fill}{align}{width}}'.format(message=entry,fill=' ',align='<',width=50) 	# entry 
       fdo.write(('%s')%(fcno))                                                                      
       fdo.write(('%s\n')%(fentry)) 
   fdo.write(('%s\n')%('  ')) 
 print "===================================================================================================================================\n"
 print "\n"
 print " BUFR Codefig output - ",fdict_out,"\n"
 print "\n"
 print "===================================================================================================================================\n"
#
####################################################################################################################################################################
# subroutine name: link_common_tables
#
# purpose:         Cross reference the entries in the merged data dictionary with the Common Code Tables
#
# input:           mfdict     -  Merged codefig tables
#
# output:          mdict_out  -  Merged codefig tables with entries from the common code tables.   
#                    
#
####################################################################################################################################################################
#
def link_common_tables(mfdict2,hdr):
#
                                       # note this a listing of the relationship between 
                                       # the common code tables and the codefig tables
                                       # and as such may be subject to change at a later date
                                       # format of the list is as follows :-
                                       # link[Common Code Table identifier]=[CodeFig Description, Code Flag Number]
 link={}
 link["05"]=["SATELLITE IDENTIFIER","001007"]
 link["07"]=["TRACKING TECHNIQUE/SYSTEM STATUS","002014"]
 link["08"]=["SATELLITE INSTRUMENTS ","002019"]

 link["01"]=["ORIGINATING/GENERATING CENTRE (SEE 001031)","001033"]
 link["02"]=["RADIOSONDE TYPE ","002011"]

#
#########################################
#
# take dir listing of the common code tables in /common_ct
# append to a list (file_list)
#
 file_list=[]
 cf_dict={}
 psd=os.getcwd()
# this will need changing as will reference different directory !!!!!
# suggest passing as variable from shell
 psf=hdr+"/common_ct/" 
 for filename in os.listdir(psf):
   file_list.append(filename)                                                                  # will produce a directory listing of /common_ct
#                                                                                              # and append to file_list
 match=[]
 master=[] 
 for key in link.keys():
    tab=key
    s_pattern="Common_C"+tab
    match = [s for s in file_list if s_pattern in s]
    master.append(match)                                                                       # compile list of files which match the pattern s_pattern
 mlen=len(master)                                                                              # append details to list master
#
 master2=[]
 masterf=[]
 for y in range(0,mlen):                                                                       # open each of the files in /common_ct and extract
     fname=master[y][0]
     fname_full=psf+fname
     fname_label=fname[8:10]
     
     if fname_label == "05" :                                                                  
         c1=2
         c2=4
     elif fname_label == "08":
         c1=1
         c2=4
     elif fname_label == "07":
         c1=2
         c2=3
     else:
         c1=3
         c2=4
     with open (fname_full,'r') as fr:
       first_ent=[]
       first_ent=link[fname_label][1]
       ftitle=link[fname_label][0]
       line=fr.next()                                                                            # skip the first line - has column titles etc  
       master2=[]
#       
       master2.append(ftitle)
       masterf.append(master2)       
       master2=[]
#      
       for line in fr:
          list1=[]
          sl=line.replace("\"","")                                                              # remove inverted commas
          sline=sl.split(",")
          sline2=[x for x in sline if x]                                                        # remove blank entries from the list
          if len(sline2) <= 3:
            line=fr.next()
          else:
            if fname_label == "08":
               code_num=sline[c1]
               code_desc=sline[c2]
            else:
               code_num=sline[c1]
               code_desc=sline[c2]
            list1.append(code_num)
            list1.append(code_desc) 
            masterf.append(list1) 
#            
       out_key=link[fname_label][1]
       cf_dict[out_key]=masterf  
       masterf=[]
     fr.close()                                                                                  # note: - cf_dict contains the entries from the common code tables
 for key in cf_dict.keys():
    key2=key
    tlist=[]
    if key2 in mfdict2:
       tlist2=cf_dict[key2]
       mfdict2[key2]=tlist2       
 return(mfdict2)  
#
####################################################################################################################################################################
# subroutine name: comp_code_dicts
#
# purpose:         Run comparison of the new and old codefig files
#
# input:           dict1     -  Current MetDB codefig tables (dictionary of lists)
#                  dict2     -  New Code Fig tables downloaded from the WMO Web Pages
#
# returns:          mdict_out  -  Merged codefig tables with entries from the common code tables.   
#                    
#
####################################################################################################################################################################
#
def comp_code_dicts(dict1,dict2):
#
# dict1 => old dictionary
# dict2 => new dictionary
#
 list_old=[]
 list_new=[]
 diff_old={}
 diff_new={}
 for key in dict1:          # Comparison of old code table with new
    if key in dict2:
       list_new=dict2[key]
       list_old=dict1[key]
       diff_l=[x for x in list_old if not x in list_new]  
       if len(diff_l) > 0:
          diff_old[key]=diff_l
    else:
       diff_old[key]=dict1[key]
#
 for key in dict2:          # Comparison of old code table with new
    if key in dict1:
       list_new=dict1[key]
       list_old=dict2[key]
       diff_l=[x for x in list_new if not x in list_old]  
       if len(diff_l) > 0:
          diff_new[key]=diff_l
    else:
       diff_new[key]=dict2[key]
 return()
#
####################################################################################################################################################################
# subroutine name: rep_d
#
# purpose:         Expand all D sequences, replacing nested D sequences with full listings of B Descriptors
#
# input:           odictn     -  Dict of D Sequences.
#
# output:          odictf  -   Dictionary containing expanded D sequence.
#                    
#
####################################################################################################################################################################
#
def rep_d(odictn):
# expand sequence and 
# replace table D - "3" descriptors with full listings
 odictf={}
 for key in odictn.keys():
   ilistd=[]
   ilistd=odictn[key][2:]
   D_test="True"
   while D_test:         # this loop will repeat until no 3 descriptors are found
         restart = False
         n=0
         xylen=len(ilistd)
         for i in range (0,xylen):
             n=i
             ts = ilistd[i][0]
             tsf = ilistd[i]
             if ts == "3":
                tsfi=tsf
                nv=odictn[tsfi][2:]
                ilistd[i]=nv                                     #       insert at a specific point in the list                 
                if n == xylen: 
                   restart = False
                else:
                   restart = True
                   ilistdo=[]
                   ilistdo=print_list(ilistd,ilistdo) 
                   ilistd=ilistdo
                   break
         if not restart:  
            odictf[key] = ilistd   
            break  
 return (odictf)

#
####################################################################################################################################################################
# subroutine name: print_list
#
# purpose:         Subroutine called by "straight" to identify and replace nested lists. The instance method will return true 
#                  if the object matches the type specified , in this case "List".
#                  therefore a nested list such as -
#
#                                 ["23","24","25",["26","27","28"],"29","30"]
#                                        will be transformed to 
#                                 ["23","24","25","26","27","28","29","30"]         
#
# input:           the_list    -  list which may (or may not) contain nested lists
#
# returns:        dn          -  output lists
#                    
#
####################################################################################################################################################################
#
def print_list(the_list,dn):  
  for each in the_list:
      if isinstance(each, list):
          print_list(each,dn)
      else:
         dn.append(each) 
  return(dn)
#
####################################################################################################################################################################
# subroutine name: straight
#
# purpose:         Will search for nested lists (within the main list) and merge the nested 
#                  data at the right point. The end result will be one continous list. Note this 
#                  subroutine includes a call to the subroutine print_list 
#
# input:           idict    -  expanded D sequence returned by rep_d. 
#
# returns:         tempd      - dictionary of lists corresponding to an expanded Table D Sequence
#                               cross referenced against Tables B & C..  
#                    
#
####################################################################################################################################################################
#
def straight(idict):
  for key in idict.keys():
    tdfn=[]
    tdfo=[]
    tdfn=idict[key]
    tdf= print_list(tdfn,tdfo)
    idict[key]=tdf 
  return (idict)
#
####################################################################################################################################################################
# subroutine name: read_tablec
#
# purpose:         read in the contents of BUFR Table C to a dictionary of lists. The dictionary will be of the format -
#                  tabc{key :[ 'Descriptor' ,'Details',' ',' ']}
#
# input:           cin       -  text version of Table C
#
# returns:         tabc      - dictionary of lists corresponding to Table C
#                    
#
####################################################################################################################################################################
#
def read_tablec(cin):
   tabc={}
   clist=[]
   tdc=open(cin,'r') 
   for line in tdc: 
     ctemp=[]
     at=line.replace(";"," ")
     a=at.replace("\",,,\"",";;;")
     b=a.replace("\",,\"",";;")
     c=b.replace("\",\"",";")
     d=c.replace("\",",";")
     e=d.replace(",\"",";")
     cline=e
     clines=cline.split(";") 
     key=clines[1]
     cdesc=clines[2]
     cdetails=clines[3]
     blank1="na"
     ctemp.append(cdesc)
     ctemp.append(cdetails)  
     ctemp.append(blank1) 
     ctemp.append(blank1) 
     tabc[key]= ctemp    
   return (tabc)
#
####################################################################################################################################################################
# subroutine name: cross_ref
#
# purpose:         cross_reference an expanded D sequence against BUFR Table C
#
# input:           bdictin    -  dictionary of lists containing Table B
#                  ddictin    -  dictionary of lists containing Table D
#                  cdictin    -  dictionary of lists containing Table c
#
# returns:         tempd      - dictionary of lists corresponding to an expanded Table D Sequence
#                               cross referenced against Tables B & C..  
#                    
#
####################################################################################################################################################################
#
def cross_ref(bdictin,ddictin,cdictin):
  tempd={}
  for key in ddictin.keys():
      tlist=[]
      tlist=ddictin[key]
      tlen=len(tlist)
      tent=[] 
      for y in range(0,tlen):
        bdet=[]
        bkey=tlist[y]                                                     # type => str
     #   bkeyt=int(bkey)                                                   # type => int
        bkeyt=bkey
        if bkey[0] == "2":                               # check for table C operator
           bkeyf=int(bkey[:3])
           bent=[] 
           if bkeyf >= 200 and bkeyf < 222 : 
              bnval=bkey[3:]
              bnkey=bkey[:3]+"YYY"
              bdet=cdictin[bnkey]
              bdets=bdet[1]
              bdetn=bdets.replace("YYY",bnval)
              #
              bent.append(bkey)
              bent.append(bdetn)
              bent.append(bdet[2])
              bent.append(bdet[3]) 
              tent.append(bent)
           else:
              bdet=cdictin[bkeyt]
              tent.append(bkey)
              tent.append(bdet)  
        elif bkey[0] == "1" :                          # replication inc delayed   
           bdet=[]
           descno=bkey[1:3]
           repno=bkey[3:6]
           repnon=int(repno)
           if repnon == 0:
              title="Delayed Replication of next "+descno+" descriptors"
           else:
              title="Relication of next "+descno+" descriptors "+repno+" times"   
           bent=[]
           bent.append(bkey)           
           bent.append(title)
           tent.append(bent)
        else:                                         # normal table B operator
           bdet=bdictin[bkeyt] 
           bdesc=bdet[0][1]
           bunit=bdet[0][2]
           bscale=bdet[0][3]
           bref=bdet[0][4]
           bent=[]
           bent.append(bkey)
           bent.append(bdesc)
           bent.append(bunit) 
           bent.append(bscale)
           bent.append(bref)         
           tent.append(bent) 
      tempd[key]=tent 
  return (tempd)
#
####################################################################################################################################################################
# subroutine name: out_seq
#
# purpose:         Output Expanded Sequence
#
# input:           fdict      -  Expanded D Sequence , cross referenced against Table C. Note this is output from cross_ref
#                  seqID      -  D Sequence which has been expanded.
#
# returns:          Nil.  
#
# Output:          D_Sequence_Sequence_ID_output_YYDDMM_HHMM.txt 
#                    
#
####################################################################################################################################################################
#
def out_seq(fdict,seqID,build_dir):
   i=datetime.now()
   xt=i.strftime('%Y-%m-%d_%H:%M:%S')
   xp=i.strftime('%d/%m/%Y %H:%M:%S')
   pwd=os.getcwd() 
   fcs_out=build_dir+"/output/D_Sequence_"+seqID+"_output_"+xt+".txt" 
   fcs=open(fcs_out,'w') #
 #  seqID2=int(seqID)
   seqID2=seqID
   for key in fdict.keys():
      ab=type(key)
   seq_out=[]
   seq_out=fdict[seqID2]
   slen=len(seq_out)
   mkey_out='{message:{fill}{align}{width}}'.format(message=seqID,fill=' ',align='<',width=15)
   fcs.write(('%s\n') % (mkey_out))
   for x in range(0,slen):
      keyd=seq_out[x][0]
      dID=seq_out[x][1]
      key_out='{message:{fill}{align}{width}}'.format(message=keyd,fill=' ',align='<',width=15)
      dID_out='{message:{fill}{align}{width}}'.format(message=dID,fill=' ',align='<',width=40)
      fcs.write(('%s') % (key_out))
      fcs.write(('%s\n') % (dID_out))
   fcs.close()
   print "===================================================================================================================================\n"
   print "\n"
   print " D Sequence ",seqID," Expanded output to - ",fcs_out,"\n"
   print "\n"
   print "===================================================================================================================================\n"
   return()  
#
####################################################################################################################################################################
# subroutine name: index_cr
#
# purpose:         Output skeleton of the elements index for a specific D sequence
#
# input:           fdict      -  Expanded D Sequence , cross referenced against Table C. Note this is output from cross_ref
#                  seqID      -  D Sequence which has been expanded.
#
# output:          Element_Index_SeqID_output_YYMMDD_HHMM.txt  
#                    
#
####################################################################################################################################################################
#
def index_cr(fdict,seqID,build_dir):
   i=datetime.now()
   xt=i.strftime('%Y-%m-%d_%H:%M:%S')
   xp=i.strftime('%d/%m/%Y %H:%M:%S')
   SeqID=str(seqID)
   pwd=os.getcwd()   
   out_file2=build_dir+"/output/Element_Index_"+SeqID+"_output_"+xt+".txt" 
   seq_in=[]
 #  seqID2=int(seqID)
   seqID2=seqID
   seq_in=fdict[seqID2] 
   seq_out=[]
   si_len=len(seq_in)
   with open(out_file2,'w') as q:
     seg=[]
     segf=[]
     pos=1 
     repn=1
     segn=1
     for k in range(0,si_len) :
       desc=seq_in[k]
 
       keyi=desc[0]
 
       if keyi[0]== "1":                                     #  replication ind signifies segment division 
          pos = 1
          segn += 1
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          desc.append(repn) 
          repn += 1                                          #  increment the replication number
          repc=int(keyi[3:])
          if repc == 0:                                      #  expect delayed replication
             k += 1                                          #  increment count to force read of next desc  
             desc2=[]
             desc2=seq_in[k]
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
          else:                                                # fixed replication
             desc.append(repc) 
             seq_out.append(desc) 
       elif  keyi[0] == "2":                                    # descriptor is an operator
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          desc.append(" ")
          seq_out.append(desc) 
       elif  keyi[0] == "0" and keyi[:3] != "031" :              # standard descriptor
             pos2=str(pos)
             desc.append(pos2)
             desc.append(segn)
             desc.append(" ")
             desc.append(" ")
             seq_out.append(desc) 
             pos += 1 
     dcpt='{message:{fill}{align}{width}}'.format(message="Descriptor",fill=' ',align='<',width=14)
     dta='{message:{fill}{align}{width}}'.format(message="Data",fill=' ',align='<',width=84)
     sgmt='{message:{fill}{align}{width}}'.format(message="Segment",fill=' ',align='<',width=10)
     pstn='{message:{fill}{align}{width}}'.format(message="Position",fill=' ',align='<',width=11)
     rptn='{message:{fill}{align}{width}}'.format(message="Replication",fill=' ',align='<',width=14)
     count='{message:{fill}{align}{width}}'.format(message="Count",fill=' ',align='<',width=8)
     linespace='{message:{fill}{align}{width}}'.format(message="=",fill='=',align='>',width=140)
     q.write(('%s') % (dcpt))                         # start titles 
     q.write(('%s') % (dta))
     q.write(('%s') % (sgmt))
     q.write(('%s') % (pstn))
     q.write(('%s') % (rptn))
     q.write(('%s\n') % (count))
     q.write(('%s\n') % (linespace))                    # and underline
     for t in range(0,si_len): 
         keyd=seq_out[t][0]
         dscrip=seq_out[t][1]
         if len(dscrip) >= 45:
            dscrip=dscrip[0:44] 
         segmnt=seq_out[t][6]
         posnt=seq_out[t][5]
         repn=seq_out[t][7]
         count=seq_out[t][8]
         key_out='{message:{fill}{align}{width}}'.format(message=keyd,fill=' ',align='<',width=14)      
         desc_out='{message:{fill}{align}{width}}'.format(message=dscrip,fill=' ',align='<',width=86)
         sgmt_out='{message:{fill}{align}{width}}'.format(message=segmnt,fill=' ',align='<',width=10)
         pstn_out='{message:{fill}{align}{width}}'.format(message=posnt,fill=' ',align='<',width=11)
         rptn_out='{message:{fill}{align}{width}}'.format(message=repn,fill=' ',align='<',width=14)
         count_out='{message:{fill}{align}{width}}'.format(message=count,fill=' ',align='<',width=8)
         line_out='{message:{fill}{align}{width}}'.format(message=linespace,fill=' ',align='<',width=14)
         if keyd[0] == "1":                           # descriptor implies replication
            q.write(('%s') % (key_out))
            q.write(('%s') % (desc_out))
            q.write(('%s') % (sgmt_out))
            q.write(('%s') % (pstn_out))
            q.write(('%s') % (rptn_out))
            q.write(('%s\n') % (count_out))
            q.write(('%s\n') % (linespace))   
            if keyd[3:] == "000":                    # count signifies a delayed replication
               t += 1
               keyd=seq_out[t][0]
               dscrip=seq_out[t][1]
               segmnt=seq_out[t][6]
               posnt=seq_out[t][5]
               repn=seq_out[t][7]
               count=seq_out[t][8] 
               key_out='{message:{fill}{align}{width}}'.format(message=keyd,fill=' ',align='<',width=14)      
               desc_out='{message:{fill}{align}{width}}'.format(message=dscrip,fill=' ',align='<',width=86)
               sgmt_out='{message:{fill}{align}{width}}'.format(message=segmnt,fill=' ',align='<',width=10)
               pstn_out='{message:{fill}{align}{width}}'.format(message=posnt,fill=' ',align='<',width=11)
               rptn_out='{message:{fill}{align}{width}}'.format(message=repn,fill=' ',align='<',width=14)
               count_out='{message:{fill}{align}{width}}'.format(message=count,fill=' ',align='<',width=8)
               q.write(('%s') % (key_out))
               q.write(('%s') % (desc_out))
               q.write(('%s') % (sgmt_out))
               q.write(('%s') % (pstn_out))
               q.write(('%s') % (rptn_out))
               q.write(('%s\n') % (count_out))
               q.write(('%s\n') % (linespace))   
            else:                                     #standard replication
               line_out='{message:{fill}{align}{width}}'.format(message=linespace,fill=' ',align='<',width=14) 
               q.write(('%s\n') % (linespace)) 
         elif keyd[:3] != "031" and keyd[0] == "0":                                         # standard descriptor
            q.write(('%s') % (key_out))
            q.write(('%s') % (desc_out))
            q.write(('%s') % (sgmt_out))
            q.write(('%s') % (pstn_out))
            q.write(('%s') % (rptn_out))
            q.write(('%s\n') % (count_out))
   q.close()
   print "===================================================================================================================================\n"
   print "\n"
   print " Element Index output - ",out_file2,"\n"
   print "\n"
   print "===================================================================================================================================\n"
   return()  
#
####################################################################################################################################################################
# subroutine name: menu
#
# purpose:         Display options and gather input from the user. 
#
# input:           Option no - as below  
#                    
#
####################################################################################################################################################################

def menu(opti):
    inp=opti[0]                                                            # option indicator 
    hdiri=opti[1]                                                          # root of the bufr directory
    tablegBx=opti[2]                                                       # WMO version of BUFR Table B (txt)
    tablegDx=opti[3]                                                       # WMO version of BUFR Table D (txt)
    tablegCx=opti[4]                                                       # WMO version of BUFR Table C (txt)  
    tablegcct=opti[5]                                                      # WMO version of BUFR code and flag tables (txt) 
    tablegcc1=opti[6]                                                      # WMO version of Common Code tables No (1)
    tablegcc2=opti[7]                                                      # WMO version of Common Code tables No (2)  
    tablegcc5=opti[8]                                                      # WMO version of Common Code tables No (5)
    tablegc7=opti[9]                                                       # WMO version of Common Code tables No (7) 
    tablegc8=opti[10]                                                      # WMO version of Common Code tables No (8)   
    tableoBt=opti[11]                                                      # MetDB version of BUFR Table B (txt)
    tableoDt=opti[12]                                                      # MetDB version of BUFR Table D (txt)
    tableglct=opti[13]                                                     # MetDB version of the codefig tables (txt) 
    seq=opti[14]                                                           # Table D ID 
    print "seq ",seq,"\n"                                                       
#    
    if inp=="3":                                                           # Update operational BUFR table B with new local entry 
        Tno="B"                                 
        table_check(inp,hdiri,tablegBx,tableoBt,tablegCx)                                   
	(mastergb,wmo_version)=getDatb(tablegBx)                                                                                                      
	(masterlb,rev_now)=read_in_tableb(tableoBt)                                                                      
	(f_dictb)=final_dictb(masterlb,mastergb,wmo_version)                                                  
	output_b(f_dictb,rev_now,wmo_version,hdiri)	
	comp_versions(mastergb,masterlb,Tno,wmo_version,rev_now,hdiri)  	
#					              
    elif inp == "4":                                                        # Update current operational version of Table D from New WMO version
       Tno="D"
       table_check(inp,hdiri,tablegDx,tableoDt,tablegCx)                        
       (mastergd,wmo_version)=getDatd(tablegDx)
       (masterld,rev_now)=read_in_tableD(tableoDt)
       (f_dictd)=final_dictd(masterld,mastergd)  
       output_d(f_dictd,rev_now,wmo_version,hdiri)      
       comp_versions(mastergd,masterld,Tno,wmo_version,rev_now,hdiri)                         
#  
    elif inp == "1":                                                        # Update current operational version of Table B from New WMO version 
        Tno="B"
        table_check(inp,hdiri,tablegBx,tableoBt,tablegCx)         
	(mastergb,wmo_version)=getDatb(tablegBx)            
	(masterlb,rev_now)=read_in_tableb(tableoBt)                           
	new_entry=add_entry(Tno,rev_now,masterlb) 
	(f_dictb)=final_dictb(new_entry,mastergb,wmo_version)   
	output_b(f_dictb,rev_now,wmo_version,hdiri)	        							       							           
# 
    elif inp == "2":                                                       # Update operational BUFR table D with new local entry 
        Tno="D"
	table_check(inp,hdiri,tablegDx,tableoDt,tablegCx)         
	(mastergd,wmo_version)=getDatd(tablegDx)
	(masterld,revd)=read_in_tableD(tableoDt)
	(new_entry)=add_entry(Tno,revd,masterld) 
	(f_dictd)=final_dictd(new_entry,mastergd)   
	output_d(f_dictd,revd,wmo_version,hdiri)    
#  
    elif inp == "6":                                                       # compare current operational and latest WMO versions of Table B
        Tno="B"
        table_check(inp,hdiri,tablegBx,tableoBt,tablegCx)           
	(masterg,newversion)=getDatb(tablegBx) 
	(masterb,rev_now)=read_in_tableb(tableoBt)
	comp_versions(masterg,masterb,Tno,newversion,rev_now,hdiri)      
#  
    elif inp == "7":                                                        # compare current operational and latest WMO versions of Table D
        Tno="D"
        table_check(inp,hdiri,tablegDx,tableoDt,tablegCx)        
	(nvers,new_version)=getDatd(tablegDx) 
	(overs,revd)=read_in_tableD(tableoDt)
	comp_versions(nvers,overs,Tno,new_version,revd,hdiri) 
# 
    elif inp == "5":                                                        # Update current operational version of Codefigs from New WMO version 
        table_check(inp,hdiri,tablegcct,tableglct,tablegcct)         
        (fdict1,rev_no)=read_oldfig(tableglct)
        fdict2=read_newfig(tablegcct)
        mfdict=mdict(fdict1,fdict2)   
        mf_dict=link_common_tables(mfdict,hdiri)
        out_codefig(mf_dict,rev_no,hdiri)    
        #        
    elif inp == "8":                                                        # Expand a D sequence to constituent B descriptors
       table_check(inp,hdiri,tablegDx,tablegBx,tablegCx)             
       (tableb,revb)=read_in_tableb(tableoBt)                             
       (tablec)=read_tablec(tablegCx)                                      
       (tabled,revd)=read_in_tableD(tableoDt)                             
       (tabled_exp)=rep_d(tabled)                                        
       tabled_final=straight(tabled_exp)                                                                                                       
       dictf=cross_ref(tableb,tabled_final,tablec)                          
       out_seq(dictf,seq,hdiri)    
#                                            
    elif inp == "9":                                                        # Expand a D sequence to constituent B descriptors and map onto draft of the elements index.
       table_check(inp,hdiri,tablegDx,tablegBx,tablegCx)              
       (tableb,revb)=read_in_tableb(tableoBt)                             
       (tablec)=read_tablec(tablegCx)                                      
       (tabled,revd)=read_in_tableD(tableoDt)                             
       (tabled_exp)=rep_d(tabled)                                        
       tabled_final=straight(tabled_exp)                                                                                                       
       dictf=cross_ref(tableb,tabled_final,tablec)                       
       out_seq(dictf,seq,hdiri)                                                
       index_cr(dictf,seq,hdiri)
#                                               
    elif inp == "10":
        sys.exit()
    else:
         print("\n Not a valid option, Try again")
    						 
####################################################################################################################################################################
menu(sys.argv[1:])
