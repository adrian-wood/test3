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
import jinja2
from jinja2 import FileSystemLoader, Environment
from jinja2 import Template
loader = jinja2.FileSystemLoader(searchpath="/var/moods/rogue_d_seq/template_ref/")
env = jinja2.Environment(loader=loader)

filein = '/var/moods/rogue_d_seq/rogue_d_seq.txt'
ddict = {}
tdict = {}
ddet = {}
#
with open(filein,'r') as fmx:
   for line in fmx: 
      if line.find('Data Type') == -1 and line.find('Run Time') == -1 :           
         lnj = ' '.join(line.split())   
         ln2 = lnj.split(',')
         keyx = ln2[1]  
         keyx2 = keyx.strip()
         keyxp = ln2[0].strip() 
         if keyxp == '1':  
            inlist = ln2[2:]
         elif keyxp == '2': 
            inlist = ln2[2] 
         elif keyxp == '3': 
            inlist = ln2[2] 
         elif keyxp == '4': 
            inlist = ln2[2:] 
         elif keyxp == '5': 
            inlist = ln2[2:] 	 	 	  
         if keyx2 in ddict:
            tfn = ddict[keyx2] 
            tfn.append(inlist)
	    ddict[keyx2] = tfn
         else:
            tfn2 = []
   	    tfn = inlist
	    tfn2.append(tfn)
	    ddict[keyx2] = tfn2
#  
      if line.find('Data Type') > -1:
         lnz2 = line.split(',')
         if lnz2[0] == 'Data Type':
            dtp = lnz2[1]
	    ddir = lnz2[3] 
	    dt1 =  lnz2[4]
	    dt2 =  lnz2[5]
	    dt3 =  lnz2[6]
	    dt4 =  lnz2[7]
	    dtf = 'From: ' + dt1 + ' ' + dt2 + '  -  ' + 'To: ' + dt3 + ' ' + dt4
	    dft = []
	    dft.append(ddir)
	    dft.append(dtf)
            ddet[dtp] = dft	   
      elif line.find('Run Time') > -1:
         lnz2 = line.split('-')	    
         rnt =  'Code executed at ' + lnz2[1]  	   	 
   fmx.close()    

   topl = 'template_d_sequence2.html' 
   topl2 = env.get_template(topl)   
   oplm = '/var/www/html/error_storage.html' 
   outputm = topl2.render(ddict1=ddict,ddict2=ddet,extp=rnt)
   with open(oplm,'w') as flm:
      flm.write(outputm)
   flm.close() 
