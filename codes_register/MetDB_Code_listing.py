# -*- coding: iso-8859-1 -*-
import sys
import binascii
import datetime
from operator import itemgetter
import glob
import os.path
from os import path
import re
import string
import shlex, subprocess
##############################################################################################################################################################################################################
# Def: read_file_details 
#
# purpose: Read *.F90,*.f90 & *.c files , transfer 
#          details to a dict of lists
#  
# input:  flistin -  File listing ( as a PYTHON list)
#                    of the source directory using a 
#                    'F90', f90, *.c search patterns
#         flistin2 - File listing of the 'mods' 
#                    source directory, as before
#                    as a PYTHON list
#
# output: filefull - dict of lists as below :-
#                    key  - code title minus '.f90' etc
#		     list - [[full code name],
#                    [code content line1,line2,line3...]]
#
#         filefullm- dict of lists as below :-
#                    key  - code title plus '_mod.f90' etc
#		     list - [[full code name],
#                    [code content line1,line2,line3...]]
#        
#                                   
##############################################################################################################################################################################################################
#
def read_file_details(flistin,flistin2,fdir1,fdir2):
 filefull={} 
 filefullm={} 
 flen=len(flistin)  
 for x in range(0,flen):
    tstf=flistin[x]                     # full filename 
    tstf2=fdir1+tstf                    # full filename & dir path    
    tlist=[]  
    with open(tstf2,'r') as fi: 
       if tstf.find(".f90") > -1:
          ind=tstf.find(".f90")
          fname=tstf[:ind]         
       elif tstf.find(".F90") > -1:
          ind=tstf.find(".F90")
          fname=tstf[:ind]  
       elif tstf.find(".c") > -1:
          ind=tstf.find(".c")
          fname=tstf[:ind]  
       tlist2=[]
       tlist2.append(tstf)              # append filename to temp list
       tlist.append(tlist2)             # append list containing filenae to output list 
       tlist3=[]   
       for line in fi:
          linec=line.strip("\n")  
          tlist3.append(linec)          # read in code listing line by line  
       tlist.append(tlist3)             # and append to second list
       filefull[fname] = tlist 
    fi.close()
#
 flen2=len(flistin2)  
 for y in range(0,flen2):
    tstm=flistin2[y]                     # full filename 
    tstm2=fdir2+tstm                     # full filename & dir path    
    tlistm=[]
    with open(tstm2,'r') as fy:    
       if tstm.find(".f90") > -1:
          indm=tstm.find(".f90")
          fnamem=tstm[:indm]         
       elif tstm.find(".F90") > -1:
          indm=tstm.find(".F90")
          fnamem=tstm[:indm]
       tlistm2=[]
       tlistm2.append(tstm)               # append filename to temp list
       tlistm.append(tlistm2)             # append list containing filenae to output list 
       tlistm3=[]         
       for line in fy:
          linem=line.strip("\n")  
          tlistm3.append(linem)           # read in code listing line by line  
       tlistm.append(tlistm3)             # and append to second list
       filefullm[fnamem] = tlistm 
    fy.close()
#    
 return(filefull,filefullm)
#
##############################################################################################################################################################################################################
#
# Def: tree_dr
#
# Purpose:  Compiles code dependencies from the code  
#           listing and appends as a python list to 
#           the end of the relevant entry in the master list
#
# input :   indict: Dict of nested lists returned 
#           from the call to 'DEF read_file_details'
#           Entries in the input dictionary will be of the 
#           form :
#           code  :{ [['code.f90'], ['listing of the subroutine ']]}  
#                       
# output : indict: Dict of nested lists to which a seperate
#          list of code dependencies have been added  
#          code  :{ [['code.f90'], ['listing of the subroutine '],
#          ['findpath', 'readcf', 'value', 'mdb_abort']]}
#                   
##############################################################################################################################################################################################################
#
def tree_dr(indict,mdict):
   linkd = {}
   list1 = [] 
   nmod = []
   for key in indict.keys():                                                       # iterate through listing of available '*.f90 ' / '*.F90' files                 
       list1=indict[key][1]                                                        # element 1 of the list => the code listing 
       len1=len(list1) 
       dep=[]                                                                      # list of code dependencies   
       for x in range(0,len1):                                                
           strtst = list1[x].lower()                                               # check file listing for occurences 'use' and '_mod'
           if strtst.find('use') > -1 and strtst.find('_mod') > -1 and strtst[0:3].find('!') == -1:               
              ind1 = strtst.find('use ')+4 
              ind2 = strtst.find('_mod')   
              enta=strtst[ind1:ind2]                                               # enta => dependency filename from 'use' line minus '_mod'                              
              entf=strtst[ind1:ind2+4]                                             # entf => dependency filename from 'use' line including '_mod' 
              if key != enta :                                                     # The name of the dependency does not equals that of the filename
                 if enta in indict.keys():                                         # dependency entry is in filelisting   
                    if enta not in dep:                                            # dependency entry is not in output list (new entry) 
                       dep.append(enta)                                            # append entry to output lis
                 else:
                    if entf in mdict.keys():                                       # if dependency filename is not in the filelisting but is       
                       dep.append(entf)                                            # in the '_mod' list add to output list    
                       entm = mdict[entf]
                       entm.append(nmod)
                       indict[entf] = entm                                         # update indict with 'mod' dependencies 
                 if enta not in indict.keys() and entf not in mdict.keys():
                    print "unidentified dependency for => ",key," ",entf,"\n"
       listf = indict[key]      
       listf.insert((len(listf)),dep)
       indict[key]=listf  
       listf = []     
   return(indict)    
# 
##########################################################################################################################################################################################
#
# Def:         read_tohtml
#
# Purpose:     Create dependency diagrams for each of the source code files. 
#               
# input  :     fdin2 =>  dictionary elements are constructed as follows -
#              fdin2[key] = [[ code dependencies],[full code name (inc extension]]
#
#              fdin3 =>  dictionary of *.mod files the elements of which 
#              are constructed as follows -
#              fdin3[key] = [[full code name (inc extension],[code listing]]
#
#              tstamp3 = Time / date stamp constructed to annotate each page
#                       
# output :     html page
#
# Comments:    The code dependencies in the fdin2 input dictionary are each 
#              annotated with a "-n" where n indicates the nesting level
#              within the hierarchy tree.
#              This integer is extracted and used to calculate the position 
#              off the code entry in the table output.
#
#
##############################################################################################################################################################################################################
#
def read_tohtml(fdin2,fdin3,tstamp3): 
   tcols = 0                                            # note applies to the width of the cell
   for key in fdin2.keys():
      if len(fdin2[key][0]) > 0:         
         to_file = '/home/h01/usmdb/public_html/moods/code_dep/output/'+ key +'_depend.html'  
         to_filel = to_file.lower()
         to_file = to_filel
         with open(to_file,'w') as fr:
               dep1 = fdin2[key][0]                     # list of dependecies       
               fname = fdin2[key][1][0]                 # filename inc ext       
               mlev = 0       
               for xc in range(0,len(dep1)):            # calculate the max nesting level from the code dependencies  
                   fg = dep1[xc].find('-')
                   lev = int(dep1[xc][fg+1:]) 
                   if lev > mlev :
                      mlev = lev                        # mlev = max nesting level
#
               clev = 0 
               for yx in range(0,len(dep1)):            # calculate max column width (chars) 
                  ps = dep1[yx].find('-')
                  pfn = dep1[yx][:ps]
                  if pfn in fdin2.keys():
                     fnl = len(fdin2[pfn][1][0])
                     if fnl > clev:
                        clev = fnl
               colw = ((clev+3) * 8)                    # single column width (pixels) = max no of chars in a filename +3 * 8
#    
               mpxwf = str(colw)+'px;'
               ncol = mlev +1                           # number of cols + 1 
               fh3 = (colw * (ncol-1))+150              # table width 
               fh4 = str(fh3)+'px;'                                        
               #                     
               mcl = 0
#              
               t1cw = '150px;'                                                      # width table1 col1  
               t2cw = str(colw * (ncol-1)+((ncol+1)*4))+'px;'                       # width table1 col2                             
               fr.write(('%s\n')%('<!DOCTYPE html>'))
               fr.write(('%s\n')%('<html>'))   
               fr.write(('%s\n')%('<head>'))
               fr.write(('%s\n')%('<style>'))
               fr.write(('%s\n')%('table th , td {')) 
               fr.write(('%s\n')%('border-collapse: collapse;'))
               fr.write(('%s\n')%('}'))
               fr.write(('%s\n')%('th , td {')) 
               fr.write(('%s\n')%('padding: 2px;'))
               fr.write(('%s\n')%('text-align: left;'))
               fr.write(('%s\n')%('}')) 
            #  
               fr.write(('%s\n')%('table#t02 {'))
               fr.write(('%s\n')%(fh4))        
               fr.write(('%s\n')%('} '))
             #     
               fr.write(('%s\n')%('tr:nth-child(even){background-color: #f2f2f2}')) 
               fr.write(('%s\n')%('</style>')) 
               fr.write(('%s\n')%('</head>'))    
               fr.write(('%s\n')%('<body>'))               
             #  
               fr.write(('%s\n')%('<table id=\"t02\" >')) 
               tcom = '<td colspan=\"1\" bgcolor=\"#92a8d1\" align=\"left\" >Source Code</td>'
               tcomex = '<td colspan=\"'+str(mlev+1)+'\" bgcolor=\"#92a8d1\" align=\"left\" >MetDB - Code Register </td>'  
               tcom2 = '<td colspan=\"'+str(mlev)+'\" bgcolor=\"#92a8d1\" align=\"left\" >Code Dependencies</td>'
               pout =  '<td style = \"width:' + mpxwf + '\" align=\"left\" >'+fname+'</td>'                 # name of source code
               pout1 =  '<td style = \"width:150px;\" align=\"left\" >'+fname+'</td>'
               pout2 =  '<td style = \"width:' + mpxwf + '\" align=\"left\" ></td>'
               pout3 =  '<td style = \"width:150px;\" align=\"left\" ></td>'
               fr.write(('%s')%('<tr>'))
               fr.write(('%s\n')%(tcomex))
               fr.write(('%s\n')%('</tr>')) 
               fr.write(('%s')%('<tr>'))
               fr.write(('%s\n')%(tcom))
               fr.write(('%s\n')%(tcom2)) 
               fr.write(('%s\n')%('</tr>'))
             #
               fr.write(('%s')%('<tr>'))  
               fr.write(('%s')%(pout1))                                                             # print first line of the table
               for xy in range(0,ncol-1):
                  fr.write(('%s')%(pout2)) 
               fr.write(('%s')%('</tr>'))  
             #                                                      
               for xc in range(0,len(dep1)):
                   fg = dep1[xc].find('-')
                   plev = int(dep1[xc][fg+1:])-1                                                    # extract the '-number' from the end of the code name.
                                                                                                    # this (converted to an int) will indicate the column to place the entry in     
                   entp = dep1[xc][:fg]               
                   fr.write(('%s')%('<tr>'))
                   tdw = '<td style = \"width:' + str(mpxwf) + '\" align=\"left\" ></td>'
                   fr.write(('%s')%(pout3))
                   for xy in range(0,mlev):
                      if xy == plev:
                           entpc = dep1[xc][:fg]
                           if entpc.find('_mod') > -1:
                              entpc1 = fdin3[entpc][0][0][:-4]
                              entpc2 = fdin3[entpc][0][0]
                           else:
                              entpc1 = fdin2[entpc][1][0][:-4]
                              entpc2 = fdin2[entpc][1][0]   
                           poutc = "<td style = \"width:" + str(mpxwf) + "\" align=\"left\"  ><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/"+entpc1+".html\" target=\"_blank\" >"+entpc2+"</a></td>"
                           fr.write(('%s')%(poutc)) 
                      else:
                           fr.write(('%s')%(tdw))
                   fr.write(('%s\n')%('</tr>'))
               fr.write(('%s\n')%('</table>'))
               fr.write(('%s\n')%('<br>'))               
               fr.write(('%s\n')%(tstamp3))
               fr.write(('%s\n')%('</div>'))
               fr.write(('%s\n')%('</body>'))        
               fr.write(('%s\n')%('</html>')) 
               fr.close()   
   return()
#
##########################################################################################################################################################################################
#
# Def:              read_useage
#
# Purpose:          Construct and display main page to illustrate code useage. 
#               
# input  :          flink => : Dict of lists returned by DEF: dependu. Each entry
#                             is of the form -
#                             {[full Code Name]: [Dependencies]]
#                              example: -
#                              udict => key  tafrep.f90  : ['uaedit.f90', 'ncmbul.f90', 'bogind.f90', 
#                              'clmbul.f90', 'tafind.f90', 'srwbul.f90', 'tbusbul.f90', 'enhbul.f90']
#                   tstamp2 => Time date stamp            
#
# output :          html page summarising code useage
#                   '/home/h01/usmdb/public_html/moods/code_dep/output/c_usage_main.html'
#
##########################################################################################################################################################################################
#
def read_useage(flink,tstamp2): 
   tcols = 0                                            # note applies to the width of the cell
   olp2 = []
   aplist = []
   apld = {} 
   apl = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'] 
   to_file2 = '/home/h01/usmdb/public_html/moods/code_dep/output/c_usage_main.html'
   to_file3 = 'http://www-metdb/~usmdb/moods/code_dep/output/c_usage_main.html' 
#
   with open(to_file2,'w') as fr:                                                                                                                                           
        fr.write(('%s\n')%('<!DOCTYPE html>'))
        fr.write(('%s\n')%('<html>'))   
        fr.write(('%s\n')%('<head>'))
        fr.write(('%s\n')%('<title>MetDB Code Register - Code Useage</title>')) 
        fr.write(('%s\n')%('<style>'))
        fr.write(('%s\n')%('table th , td {')) 
        fr.write(('%s\n')%('border-collapse: collapse;'))
        fr.write(('%s\n')%('}'))
        fr.write(('%s\n')%('th , td {')) 
        fr.write(('%s\n')%('padding: 2px;'))
        fr.write(('%s\n')%('text-align: left;'))
        fr.write(('%s\n')%('}')) 
# 
        fr.write(('%s\n')%('table#t01 {'))
        fr.write(('%s\n')%('width: 100%'))   
        fr.write(('%s\n')%('}'))         
        fr.write(('%s\n')%('</style>'))
        fr.write(('%s\n')%('</head>'))
#
        fr.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        fr.write(('%s\n')%('<tr><th width=\"80%\"  bgcolor=\"#92a8d1\" ><a name=\"' + 'CN' + '\">MetDB - Code Register:  Code Usage</th></tr> '))            
#
#    
        fr.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        fr.write(('%s\n')%('<tr>'))        
        for xc in range(0,len(apl)):
           bip = '<td width=\"3%\"><a href=\"'+to_file3+'#'+ apl[xc] + '\">' + apl[xc] + '</a></td>' 
           fr.write(('%s\n')%(bip)) 
        fr.write(('%s\n')%('</tr>'))
        fr.write(('%s\n')%('</table>'))
        fr.write(('%s\n')%('<br>'))    
# 
        fr.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        fr.write(('%s\n')%('<tr><th width=\"20%\" bgcolor=\"#92a8d1\" >Code Name</th><th width=\"60%\"  bgcolor=\"#92a8d1\" >Called by</th></tr> '))
        fr.write(('%s\n')%('</table>'))                                               
#
        for n in range(0,len(apl)):
           fr.write(('%s\n')%('<br>'))
           fr.write(('%s\n')%('<table style width = \"15%\" border=\"1\" > '))
           plf = '<tr><td width="5%" bgcolor=\"#92a8d1\" ><a name=\"' + apl[n] + '\"></a>' + apl[n]+ '</td><td width="10%" bgcolor=\"#92a8d1\" ><a href=\"c_usage_main.html#'+ 'CN'+ '\">' + 'Back to Top'+'</td></tr>'
           fr.write(('%s\n')%(plf))  
           fr.write(('%s\n')%('</table>')) 
           fr.write(('%s\n')%('<br>'))       
           for key2 in sorted(flink):     
              if key2[0].upper() == apl[n]:
                 flist = flink[key2]
                 flen = len(flist)
                 if flen > 3:
                   mr = flen%4
                   if mr > 0:
                      nr = (( flen - mr)/3)+1
                   else:
                      nr = flen / 3 
                 else:
                      nr = 1
                 fprog2 = '<a href=\"'+ key2[:-4] +'.html\" target=\"_blank\" >'     
                 fr.write(('%s\n')%('<table style width = \"80%\"  > ')) 
                 plf2 = '<tr><td width=\"20%\" style=\"border-right: 3px solid #92a8d1\" >' + fprog2 + key2 + '</td>'
                 fr.write(('%s\n')%(plf2))   
                 for xy in range(0,flen):   
                     fprog = '<a href=\"'+ flist[xy][:-4] +'.html\" target=\"_blank\" >'                   
                     if xy == flen-1:                                    # last entry
                         if flen < 3:                            
                            if flen == 2:                                           
                               plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td><td width=\"20%\" > </td></tr>' 
                            elif flen == 1:
                               plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td><td width=\"20%\" > </td><td width=\"20%\" > </td></tr>'  
                         else:
                            if xy%3 == 0:                             
                               plf2 = '<tr><td width=\"20%\" style=\"border-right: 3px solid #92a8d1\"> </td><td width=\"20%\">'+ fprog + flist[xy] + '</td></tr>' 
                            elif  xy%3 == 2: 
                               plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td></tr>'
                            else:
                               plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td></tr>'           
                     elif xy%3 == 2:
                         plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td></tr>'  
                     elif xy%3 == 0:  
                         if xy == 0: 
                            plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td>' 
                         elif xy > 0:
                            plf2 = '<tr><td width=\"20%\" style=\"border-right: 3px solid #92a8d1\">  </td><td width=\"20%\">'+ fprog + flist[xy] + '</td>' 
                     else:
                         plf2 = '<td width=\"20%\">'+ fprog + flist[xy] + '</td>' 
                     fr.write(('%s\n')%(plf2))                    
                 fr.write(('%s\n')%('</table>'))
                 fr.write(('%s\n')%('<hr align=\"left\" width = \"80%\" color=\"#0033cc\"></hr>'))
        fr.write(('%s\n')%('<br>'))
        fr.write(('%s\n')%('<hr align=\"left\" width = \"80%\" color=\"#0033cc\"></hr>'))
        fr.write(('%s\n')%('<br>')) 
        fr.write(('%s\n')%(tstamp2))
   return()
#                           
##############################################################################################################################################################################################################
# Def:         read_to_main
#
# Purpose:     Construct html main page for code dependencies 
#              Where a code dependecies will include links to code dependencies 
#              diagram
#
# Input:       flind: = Dict of lists of the form 
#              codename = [[Full code name],[Code listing],[Code dependencies]]
#
#              tdstamp2: = Date time stamp
#
# Output:      HTML file - listing code name with link to a dependency diagram
#
#              /var/www/html/moods/misc/comp_output/c_depend_main.html
#           
# 
##########################################################################################################################################################################################
#
def read_to_main(flind,tdstamp2):
#
#
   apld = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'] 
   to_filed2 = '/home/h01/usmdb/public_html/moods/code_dep/output/c_depend_main.html'
   to_filed3 = 'http:/www-metdb/~usmdb/moods/code_dep/output/c_depend_main.html'
   #
   with open(to_filed2,'w') as frd:                                                                                                                                           
        frd.write(('%s\n')%('<!DOCTYPE html>'))
        frd.write(('%s\n')%('<html>'))   
        frd.write(('%s\n')%('<head>'))
        frd.write(('%s\n')%('<title>MetDB Code Register - Code Dependencies</title>')) 
        frd.write(('%s\n')%('<style>'))
        frd.write(('%s\n')%('table th , td {')) 
        frd.write(('%s\n')%('border-collapse: collapse;'))
        frd.write(('%s\n')%('}'))
        frd.write(('%s\n')%('th , td {')) 
        frd.write(('%s\n')%('padding: 2px;'))
        frd.write(('%s\n')%('text-align: left;'))
        frd.write(('%s\n')%('}')) 
 # 
        frd.write(('%s\n')%('table#t01 {'))
        frd.write(('%s\n')%('width: 100%'))   
        frd.write(('%s\n')%('}'))         
        frd.write(('%s\n')%('</style>'))
        frd.write(('%s\n')%('</head>'))
#
        frd.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        frd.write(('%s\n')%('<tr><th width=\"80%\"  bgcolor=\"#92a8d1\" ><a name=\"' + 'CD' + '\">MetDB - Code Register:  Code Dependencies</th></tr> '))                
        frd.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        frd.write(('%s\n')%('<tr>'))        
        for xc in range(0,len(apld)):
           bip = '<td width=\"3%\"><a href=\"c_depend_main.html#'+ apld[xc] + '\">' + apld[xc] + '</a></td>' 
           frd.write(('%s\n')%(bip)) 
        frd.write(('%s\n')%('</tr>'))
        frd.write(('%s\n')%('</table>'))
        frd.write(('%s\n')%('<br>'))    
# 
        frd.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        frd.write(('%s\n')%('<tr><th width=\"20%\" bgcolor=\"#92a8d1\" >Code Name</th><th width=\"60%\"  bgcolor=\"#92a8d1\" >Code Dependencies</th></tr> '))
        frd.write(('%s\n')%('</table>'))                                               
#
        for n in range(0,len(apld)):
           frd.write(('%s\n')%('<br>'))
           frd.write(('%s\n')%('<table style width = \"15%\" border=\"1\" > '))
           pld = '<tr><td width=\"5%\" bgcolor=\"#92a8d1\" ><a name=\"' + apld[n] + '\"></a>' + apld[n]+ '</td><td width=\"10%\" bgcolor=\"#92a8d1\" ><a href=\"c_depend_main.html#'+ 'CD'+ '\">' + 'Back to Top'+'</td></tr>'
           frd.write(('%s\n')%(pld))  
           frd.write(('%s\n')%('</table>')) 
           frd.write(('%s\n')%('<br>'))       
           for key2d in sorted(flind):              
              if key2d.find('_mod') == -1:
                 if key2d[0].upper() == apld[n]:
                    frd.write(('%s\n')%('<table style width = \"80%\"  > '))
                    fnamed = flind[key2d][0][0]
                    fnamed2 = flind[key2d][0][0][:-4]  
                    fnamed3 = fnamed2.lower()
                    depn = flind[key2d][2] 
                    fprogd = '<tr><td width=\"20%\" style=\"border-right: 3px solid #92a8d1\" ><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/'+ fnamed3 +'.html\" target=\"_blank\" >'+fnamed+'</a></td>' 
                    fprogd2 = '<td width=\"5%\" >  </td><td width=\"55%\" ><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/'+ fnamed3 +'_depend.html\" target=\"_blank\" >'+fnamed+' - Dependency diagram </a></td></tr>' 
                    fprogd3 = '<td width=\"5%\" ></td> <td width=\"55%\" >No code dependencies </td></tr>'
                    frd.write(('%s')%(fprogd))
                    if len(depn) > 0:                 
                       frd.write(('%s\n')%(fprogd2))
                    else:
                       frd.write(('%s\n')%(fprogd3))
                    frd.write(('%s\n')%('<hr align=\"left\" width = \"80%\" color=\"#0033cc\"></hr>')) 
                    frd.write(('%s\n')%('</table> '))             
                    frd.write(('%s\n')%('<br>'))
        frd.write(('%s\n')%('<hr align=\"left\" width = \"80%\" color=\"#0033cc\"></hr>'))
        frd.write(('%s\n')%('<br>')) 
        frd.write(('%s\n')%(tdstamp2))
        frd.close()        
   return()  
#   
#########################################################################################################################################################################################################
# def: set_d
#
# purpose:  Establish default directories for the *.F90
#	    source code , both code and mod files.
#            
# Input  : Source_code and Source_code/mods directory 
#          Input as variables d1 & d2 
#
# Output : lbdir (List) = listing of *.F90, *.f90 and *.C files
#          d1 = source code directory (*.F90, *.f90 and *.C files)
#          d2 = source code directory (*_mod)          
#                                
##############################################################################################################################################################################################################
#
def set_d(dir1):
#  
   d1list=[]
   d2list=[] 
   d1 = dir1+'/MOODS/source/'
   d2 = dir1+'/MOODS/source/mods/'
   d1.strip()
   d2.strip()  
   d1list=os.listdir(d1)
   d2list=os.listdir(d2)   
# output listing of files with an *.F90 extension
# to a list
   list1=[x for x in d1list if  (x.find(".F90") != -1 and x.find("_mod") == -1 and x.find("_MOD") == -1)]
# output listing of files with an *.f90 extension
# to a list
   list2=[x for x in d1list if  (x.find(".f90") != -1 and x.find("_mod") == -1 and x.find("_MOD") == -1)]
# output listing of files with an *.c extension
# to a list
   list3=[x for x in d1list if  (x.find(".c") != -1)]
# output listing of files with an *._mod.f90 extension
# to a list
   list4=[x for x in d2list if  (x.find("_mod") != -1)]
#
   lbdir=list1+list2+list3
#
   return(lbdir,list4,d1,d2) 
#                                
##############################################################################################################################################################################################################
# 
#  def     : detail_dep
#
#  purpose : Expand the list of dependencies for each code listing
#            supplied in the input dictionary to include sub-dependencies.
#            The nesting level of each dependencie in relation to the calling 
#            function / subroutine will be indicated by 'code_name-n' 
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
############################################################################################################################################################################################################
def detail_dep(agh,dlp,dlm):

 xpout = [] 
 bls = dlp[2]  
 sd = dlp[0][0].find('.')  
 sdf = dlp[0][0][:sd]           
 if len(bls) == 0:          
   results2 = []
   return(results2)
 else:  
   lv = 0 
   xpout = []     
   while True:                             
      lv = lv + 1
      for xq in range(0,len(bls)):                     #   iterate through the length of the bls list      
         if bls[xq].find('-') == -1:                   #   check for the lack of  a '-' in the entry
            sdf = bls[xq]+'-'+str(lv)                  #   if not found append a '-n' to the entry
            sdfr = bls[xq]   
            xpout.append(sdf)                          #   append the modified entry to the output list
            if sdfr.find('_mod') == -1:                #   check the new entry is not a '_mod' file.       
               if sdfr in agh.keys() :                 #   '_mod' files will not have dependencies
                  dltr = agh[sdfr][2]                  #   if dependencies found append to output list               
                  if len(dltr) > 0:                   
                     xpout.append(dltr)                      
         else:
            xpout.append(bls[xq])                      #   if a '-' is found in entry append to output list  
                                                       #   the following will: 
      xpouts = str(xpout)                              #   1. convert the nested string to a string  
      xpout1 = xpouts.replace('[','')                  #   2. remove internal parenthesis '[' ']'               
      xpout2 = xpout1.replace(']','')
      xpout3 = xpout2.replace(' ','')                  #   3. replace multiple whitespace with single 
      xpoutsf = xpout3.replace('\'','')                #   4  remove ' ' ' 
      xpout4 = xpoutsf.split(',')                      #   5 split string to list on ','  
      t = 1 
      for xw in range(0,(len(xpout4))):  
         if xpout4[xw].find("-") == -1 :
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
##############################################################################################################################################################################################################
# 
#  def     :    dependu
#
#  purpose :    establish list of code dependencies for each code module.
#        
#  input   :    f5in: => Dictionary of lists of the format =
#                        Codename = [[code dependencies ],[ full code name]]
#               example :
#               'findmhs'=[['mhsiff-1', 'nextmsg-1', 'read_data-2', 'ebcdic-2'], ['findmhs.f90']]  
#
#  output  :   
#
#
############################################################################################################################################################################################################
#
def dependu(f5in):
    olf=[]
    olfp = []
    olfp2 = []
    olfd = {} 
    olfd2 = {}    
    for key in f5in.keys():                                       # entries for f5in[key] := {'code':[[code dependencies . . . . ],['code.f90']]}   
        fnm = f5in[key][1][0]
        if fnm not in olf : 
          olf.append(fnm)                                         # produce list of code filenames           
#
    for key in f5in.keys():
        fnp = f5in[key][0]
        fnp2 = f5in[key][1]
        olt = []
        olt2 = [] 
        for xt in range(0,len(fnp)):
           if fnp[xt].find('-1') > -1:
              fng = fnp[xt].find('-1') 
              olt.append(fnp[xt][:fng])
        olt2.append(olt)
        olt2.append(fnp2)
        olfd[key] = olt2                                        # olfd dict entry => { 'code': [dependency list (level 1 entries )],[full code name] ]  
    for z in range(0,len(olf)):                                  # olf => list of *.f90 filenames       
        tval = olf[z].find('.')
        tcodef = olf[z]          
        tcode = olf[z][:tval]                                    # trim file ext from code name ie file1.f90 becomes file1         
        dep1 = []  
        for kx in olfd.keys():                                   # olfd dict entry => { 'code': [dependency list (level 1 entries )],[full code name] ]          
            olfp = olfd[kx][0]                                   # extract dependency list  
            olfpk = olfd[kx][1]                                  # code name including exstention           
            if tcode in olfp:                                    # code name from olf listing found in dep list   
               if tcode in olfd2.keys():
                  dep1 = olfd2[tcode]
               dep1.append(kx)          
               olfd2[tcode]=dep1 
# 
    olfd3 = {} 
    for key in olfd2.keys():
        key2 = key
        dop2 = olfd2[key] 
        dop3 = []         
        if key2 in f5in.keys():  
           hk = f5in[key2][1][0]                                   # new key name    
        for xt in range(0,len(dop2)):
           if dop2[xt] in f5in.keys():                             # dependency list 
              dpi = f5in[dop2[xt]][1][0]    
              dop3.append(dpi)
        olfd3[hk] = dop3   
#   
    return(olfd3)
# 
##############################################################################################################################################################################################################
# 
#  def     : pagew
#
#  purpose : output indidual code listing as html page 
#        
#  input   : fdicte => Dictionary passed with the following structure 
#            Key => code name without a file extn.
#            codename[key] = [[Full code name],[Code listing....],[Subroutines / functions called]]       
#
#  output  : Listing of the code in html format. Note references to the subroutines / functions called will be html linked.
#            Output filename format - 'codename.html' 
#            Note - Returns nothing  
#  
#
############################################################################################################################################################################################################
#
def pagew(fdicte,tstamp4):
    for key in fdicte.keys():  
       fghin = fdicte[key][0]
       fghinl = fghin[0][:-4].lower()
       fdrl = 'http://www-metdb/~usmdb/moods/code_dep/output/'
       fdr = '/home/h01/usmdb/public_html/moods/code_dep/output/'
       fghout = fdr + fghinl+'.html'                         # output filename constructed from codename (- file exten) +'html'  
       with open(fghout,'w') as fp:   
          fp.write(('%s\n')%('<!DOCTYPE html>'))             # open outputfile and add intial html tages 
          fp.write(('%s\n')%('<html>'))
          fp.write(('%s\n')%('<head>'))  
          fp.write(('%s\n')%('<title>MetDB Code Register - Source Code Index: '+fghin[0]+'</title>'))    
          fp.write(('%s\n')%('<PRE STYLE=\?font-family: Georgia; font-size: 12pt; color: #000099;\?>'))  
          fp.write(('%s\n')%('</style>'))
          fp.write(('%s\n')%('</head>')) 
          fp.write(('%s\n')%("<table font-size: 12pt; cellpadding=\"4\" width=\"60%\" bgcolor=\"#92a8d1\" border =\"1\" >"))
          tcomet = '<tr><td align=\"left\" ><b>MetDB - Code Register </b></td></tr>'  
          onecell='<tr><td><b>'+fghin[0]+' Code Listing </b></td></tr>'          
          fp.write(('%s\n')%('<body>'))
          fp.write(('%s\n')%(tcomet))
          fp.write(('%s\n')%(onecell))
          fp.write(('%s\n')%('</table>'))    
          fp.write(('%s\n')%('<br>'))                        
          dflen = len(fdicte[key][1])                 
          dfin = fdicte[key][1]                        # Extract the code listing from the dict entry 
          dped = fdicte[key][2]                        # extract code dependencies   
          lno = 0  
          for xy in range(0,dflen):
              lno = lno + 1
              slno = str(lno)                          # slno will provide the line no of the code output
              lnnos = '{message:{fill}{align}{width}}'.format(message=slno,fill=' ',align='<',width = 5 )
              lnnos2 = '{message:{fill}{align}{width}}'.format(message=':! ',fill=' ',align='<',width = 3 )
              lnnos3 = '{message:{fill}{align}{width}}'.format(message=': ',fill=' ',align='<',width = 2 )             
              if dfin[xy][0:10].find('!') > 0:
                  fp.write(('%s')%(lnnos ))
                  fp.write(('%s')%(lnnos3 ))
              else:   
                  fp.write(('%s')%(lnnos ))
                  fp.write(('%s')%(lnnos2 ))   
                                                       # search for 'USE 'Codename'_mod  declarations in each line
                                                       # where found these will form a link to a code dependency    
              if  dfin[xy].find('USE') > -1 and dfin[xy].find('_mod') > -1:
                  ug1 = re.sub( '\s+', ' ', dfin[xy] ).strip()
                  lnkind = ug1.find('_mod')
                  lnop = ug1[4:lnkind]
                  lnop2 = lnop.lower()  
                  lnk = 'USE <a href=\"'+fdrl+lnop2+'.html\" target=\"_blank\" >'+lnop2+'</a>_mod'
                                  
                  fp.write(('%s\n')%(lnk))   
                                                        # search for instances of 'CALL' in the code listing. Where found 
                                                        # check against the list of code dependencies and if found construct 
                                                        # a link.                  
              elif  dfin[xy].find('USE') == -1 and dfin[xy].find('CALL ') > -1: 
                  fg1 = re.sub( '\s+', ' ', dfin[xy] ).strip()   
                  fg2 = fg1.lstrip(' ')
                  fg3 = fg2.split()
                  if 'CALL' in fg3:  
                     fin = fg3.index('CALL')
                     subn = fg3[fin+1].strip('')
                     subnp = subn.find('(')
                     if subnp > -1:
                        sunnx = subn[:subnp]
                        sunnxr = subn[subnp:]
                        sunnx2 = sunnx.lower()
                     else:
                        sunnx = subn
                        sunnx2 = sunnx.lower()                    
                     if sunnx2 in dped: 
                        lnk2 = '<a href=\"'+fdrl+sunnx2+'.html\" target=\"_blank\" >'+sunnx+'</a>'+sunnxr
                        dfg = dfin[xy].replace(subn,lnk2)
                     else:
                        dfg = dfin[xy]
                  else:
                     dfg = dfin[xy]   
                  fp.write(('%s\n')%(dfg))                                       
              else:
                  lnop = dfin[xy]
                  fp.write(('%s\n')%(lnop))
          fp.write(('%s\n')%('<br>'))        
          fp.write(('%s\n')%('<hr align=\"left\" width = \"60%\" color=\"#0033cc\"></hr>'))
          fp.write(('%s\n')%(tstamp4))         
          fp.write(('%s\n')%('</body>'))
          fp.write(('%s\n')%('</html>'))   
    return() 
# 
#############################################################################################################################################################################################################
# 
#  def     :       file_listm
#
#  purpose :       Construct master page for the source code index list.
#        
#  input   :       Dictionary of lists of the form :- 
#                  code-name:[[full code name],[code dependencies]]
#
#  output  :       main source code index (html page) listing the 
#                  source code in alphabetical order.
#  
#
############################################################################################################################################################################################################
#
def file_listm(listd,tstamp2): 
   tcols = 0                                            # note applies to the width of the cell
   olp2 = []
   aplist = []
   apld = {} 
   apl = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'] 
   to_file2m = '/home/h01/usmdb/public_html/moods/code_dep/output/file_list_index.html'
   to_file3 = 'http://www-metdb/~usmdb/moods/code_dep/output/file_list_index.html'  
   #
   with open(to_file2m,'w') as fq:                                                                                                                                           
        fq.write(('%s\n')%('<!DOCTYPE html>'))
        fq.write(('%s\n')%('<html>'))   
        fq.write(('%s\n')%('<head>'))
        fq.write(('%s\n')%('<title>MetDB Code Register - Source Code Index</title>'))  
        fq.write(('%s\n')%('<style>'))
        fq.write(('%s\n')%('table th , td {')) 
        fq.write(('%s\n')%('border-collapse: collapse;'))
        fq.write(('%s\n')%('}'))
        fq.write(('%s\n')%('th , td {')) 
        fq.write(('%s\n')%('padding: 2px;'))
        fq.write(('%s\n')%('text-align: left;'))
        fq.write(('%s\n')%('}')) 
 # 
        fq.write(('%s\n')%('table#t01 {'))
        fq.write(('%s\n')%('width: 100%'))   
        fq.write(('%s\n')%('}'))         
        fq.write(('%s\n')%('</style>'))
        fq.write(('%s\n')%('</head>'))
#
        fq.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        fq.write(('%s\n')%('<tr><th width=\"80%\"  bgcolor=\"#92a8d1\" ><a name=\"' + 'CN' + '\">MetDB - Code Register:  Source code listing</th></tr> '))               
        fq.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        fq.write(('%s\n')%('<tr>'))  
#       
        fq.write(('%s\n')%('<table style width = \"80%\"  border =\"1\" >'))
        for n in range(0,len(apl)):
           bip = '<td width=\"3%\"><a href=\"'+to_file3+'#'+ apl[n] + '\">' + apl[n] + '</a></td>' 
           fq.write(('%s\n')%(bip))   
        fq.write(('%s\n')%('<br>')) 
#     
        for n in range(0,len(apl)):
           lalp = []
           for keyz in sorted(listd):             
              if keyz[0].upper() == apl[n]:
                 file_nr = listd[keyz][1][0] 
                 lalp.append(file_nr)
           apld[apl[n]] = lalp     
#        
        for key in sorted(apld):
           listp = apld[key]
           fq.write(('%s\n')%('<table style width = \"80%\"  > ')) 
           plf = '<tr><td width="5%" bgcolor=\"#92a8d1\" ><a name=\"' + key + '\"></a>' + key + '</td><td width="10%" bgcolor=\"#92a8d1\" ><a href=\"file_list_index.html#'+ 'CN'+ '\">' + 'Back to Top'+'</td></tr>'
           fq.write(('%s\n')%(plf))  
           fq.write(('%s\n')%('</table>'))
#              
           fq.write(('%s\n')%('<br>'))
           fq.write(('%s\n')%('<table style width = \"20%\"  > '))
           for xy in range(0,len(listp)):
              lplower = listp[xy][:-4].lower()  
              fprog = '<a href=\"'+ lplower +'.html\" target=\"_blank\" >'
              plf2 = '<td width=\"20%\">'+ fprog + listp[xy] + '</td><td width=\"20%\" > </td></tr>'
              fq.write(('%s\n')%(plf2)) 
           fq.write(('%s\n')%('</table>'))           
           fq.write(('%s\n')%('<br>'))              
           fq.write(('%s\n')%('<hr align=\"left\" width = \"80%\" color=\"#0033cc\"></hr>'))
        fq.write(('%s\n')%('<br>'))
        fq.write(('%s\n')%(tstamp2))
        fq.close() 
   return()
#
##############################################################################################################################################################################################################
# 
#  def     : pagem
#
#  purpose : Create (front_page) title web page 
#        
#  input   : Date / Timstamp
#
#  output  : codes_registery_main.html
#  
#
############################################################################################################################################################################################################
#    
def pagem(tstamp1):
    opage = '/home/h01/usmdb/public_html/moods/code_dep/output/codes_registery_main.html'
    with open(opage,'w') as fo:   
          fo.write(('%s\n')%('<!DOCTYPE html>'))       # open outputfile and add intial html tages 
          fo.write(('%s\n')%('<html>'))
          fo.write(('%s\n')%('<head>'))  
          rfe = '<title> codes_registery_main </title>'
          fo.write(('%s\n')%(rfe))  
          fo.write(('%s\n')%('</head><body><div class=\"container\">'))
          fo.write(('%s\n')%('<h3>MetDB Code register </h3>')) 
          fo.write(('%s\n')%('<hr align=\"left\" width = \"60%\" color=\"#0033cc\"></hr>')) 
          fo.write(('%s\n')%('<table cellpadding=\"5\" style=\"width:70%\"  border =\"0\" >'))
          fo.write(('%s\n')%('<col width=\"30%\">'))
          fo.write(('%s\n')%('<tr><td style=\"font-size:20px\"><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/file_list_index.html\" target=\"_blank\"> Code Index</td>'))
          fo.write(('%s\n')%('<td style=\"font-size:20px\"> Alphabetic Listing of source code and associated functions </td></tr>'))
          fo.write(('%s\n')%('<tr><td style=\"font-size:20px\" ><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/c_depend_main.html\" target=\"_blank\"> Code Dependency</td>'))
          fo.write(('%s\n')%('<td style=\"font-size:20px\">Alphabetic listing of source code dependencies</td></tr>'))
          fo.write(('%s\n')%('<tr> </tr>'))
          fo.write(('%s\n')%('<tr><td><a href=\"http://www-metdb/~usmdb/moods/code_dep/output/c_usage_main.html\" target=\"_blank\"> Code Usage</td>'))
          fo.write(('%s\n')%('<td style=\"font-size:20px\"> Alphabetic Listing of source code useage</td></tr>'))
          fo.write(('%s\n')%('</table >'))
          fo.write(('%s\n')%('<tr> </tr><hr align=\"left\" width = \"60%\" color=\"#0033cc\"></hr>'))
          fo.write(('%s\n')%('<br>'))
          fo.write(('%s\n')%('<textarea rows="4" cols="80">'))
          textout = 'DEVELOPMENT Notes: The Codes registery lists  \'F90\', \'f90\' & \'*.c\' code. The exception to this rule will be where direct calls are made from within the \
                     code to \'_mod\' files. '
          fo.write(('%s\n')%(textout))
          fo.write(('%s\n')%('</textarea>'))
          fo.write(('%s\n')%('<br>'))
          fo.write(('%s\n')%(tstamp1)) 
          fo.write(('%s\n')%('</body></html'))
    fo.close()      
#          
    return()      
#    
#
##############################################################################################################################################################################################################
# main (calling function)
#
# Establish current date & time for time stamp on output
#
now = datetime.datetime.now()
y1 = now.year
mn1 = now.month
d1 = now.day
h1 = now.hour
m1 = now.minute
s1 = now.second
if int(mn1) < 10:
   mn12 = '0' + str(mn1)
   mn1 = mn12
if int(d1) < 10:
   d12 = '0' + str(d1) 
   d1 = d12
if int(h1) < 10:
   h12 = '0' + str(h1)  
   h1 = h12
if int(s1) < 10:
   s12 = '0' + str(s1) 
   s1 = s12
#
tstamp = "Last updated - " + str(y1) + "-" + str(mn1) + "-" + str(d1) + " " + " " + str(h1) + ":" + str(m1) + ":" + str(s1)
#
mdfp = sys.argv[1]
(ld1,ld2,d1o,d2o)=set_d(mdfp)                                   #  ld1 - list of *.f90 *.F90 & *.c files
                                                                #  ld2 - list of mod files
                                                                #  d1o & d2o - directory paths of the above     
#
(findict,findictm) = read_file_details(ld1,ld2,d1o,d2o)         # dict of lists containg code [[filename],[file listing]]
                                                                # and indicate the level of nesting

(f2indict) = tree_dr(findict,findictm)                          # add dependency list to lists of code 
                                                                # [[filename],[file listing],[dependencies]]
#
f2final = {}
dltr = []
f5final = {}
for key in f2indict.keys():  
   kin =  f2indict[key]
   dltr = detail_dep(f2indict,kin,findictm)                     # call to detail_dep will expand the dependencie listing
                                                                # and indicate the level of nesting                                                                                                             
   dltr2 = [] 
   k2 = f2indict[key][0]
   dltr2.append(dltr)
   dltr2.append(k2)     
   f5final[key] = dltr2 
   
   
udict = dependu(f5final)

read_useage(udict,tstamp)                                # output code useage diagram 
   
read_tohtml(f5final,findictm,tstamp)                     # output individual dependencey diagrams  

pagew(f2indict,tstamp)                                   # output indidual file listings 

file_listm(f5final,tstamp)                               # index page for code file listing

pagem(tstamp)                                            # create front web page

read_to_main(f2indict,tstamp)                            # output main file listing for code dependencies
                                                         
                                                                                           