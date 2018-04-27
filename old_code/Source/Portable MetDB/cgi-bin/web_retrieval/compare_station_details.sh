#!/bin/sh
#
#-------------------------------------------------------------
# File Name:            compare_station_details.sh
# Location:             /home/us0400/httpd/cgi-bin/dev/
#
# This  cgi script was written by Stan Kellett G08, x6954
#
# Its purpose is, using the date input from a form in format yyyymmdd
# compare the latest Abreviated Station List at that date with
# the current Abreviated Station Details and return the differences
# as a resulting web page. Or return that no differences found.
#
# This cgi is submitted by Web form on page:
#                        httpd://us0400/~ussk/compare_station_details.html
# 
# Variables used:
#                DYEAR     -     Holds Year  eg 2000
#                DMONTH    -     Holds Month eg 02 for February
#                DDAY      -     Holds Day eg 01 for 1st of Month
#                context   -     Holds number of lines of context for output
#                inputdate -     Holds Date in original format input by user
#                instring  -     Holds date for rlog
#                instring2 -     Holds date for output in web heading
#
# Temperory external file:
#   /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST.TEMP - temp file deleted
#                                                      after use.
#   /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST      - File holding Abbreviated
#                                                      Station Details.
#
#-------------------------------------------------------------
# $Log:
#  2    Met_DB_Project 1.1         05/04/2007 15:02:53    Rosemary Lavery This
#       script compares the current revision of Stationmaster with an earlier
#       one.  Syntax updated for Linux.
#  1    Met_DB_Project 1.0         05/04/2007 14:09:25    Rosemary Lavery 
# $
# Revision 1.3  2006/08/29 08:30:30  usmdb
# changed mdb folder to mdb_new_folder.
# Stan Kellett.
#
# Revision 1.2  2000/03/02 10:20:27  ussk
# correction of some spelling mistakes
#
# Revision 1.1  2000/03/02  08:56:53  08:56:53  usmdb (Generic MDB account)
# Initial revision
# 
#

echo "Content-Type: text/html"
echo

# First of all manipulate input string to get in format for rlog -d

# Split up year month and day part of input
DYEAR=$(echo $QUERY_STRING | cut -c 14-17)
DMONTH=$(echo $QUERY_STRING | cut -c 18-19)
DDAY=$(echo $QUERY_STRING | cut -c 20-21)
context=$(echo $QUERY_STRING | cut -c 31-31)

# Now add them bzck together in required format
inputdate=$(echo $DYEAR$DMONTH$DDAY)
instring=$(echo $DYEAR'/'$DMONTH'/'$DDAY'>')
instring2=$(echo $DYEAR'/'$DMONTH'/'$DDAY)

# set directory to directory for station details,
# rlog command will otherwise not work.
cd /home/us0400/mdb_new/op/data

# Set revlat to hold latest revision number, and set
# revtest to be the revision number to compare against the latest revision.
revlat=$(echo `rlog /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST |awk '$1 ~/revision/ {print $2}' |head -n 1`)
revtest=$(echo `rlog -d$DYEAR'/'$DMONTH'/'$DDAY'>' /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST |awk '$1 ~/revision/ {print $2}' |head -n 1`)


# Set up the html page.
echo "<HTML>"
echo "<HEAD><TITLE>Compare Abreviated Station Details</TITLE></HEAD>"
echo "<BODY BGCOLOR=WHITE TEXT=BLACK>"
echo "<H2>Results of Comparison of Pre-"$instring2" and latest list</H2>"
echo "<BR>"
echo "If any differences found, differences will be displayed with the oldest first followed by the current operational version."
echo "<BR>"
echo "Additions will be shown with a + on the operational version."
echo "<BR>"
echo "Deletions by a - on the old version."
echo "<BR>"
echo "Changes with a ! on both versions."
echo "<BR>"

#create a temperory file for checking of old List
co -r$revtest -l /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST
cp /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST  /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST.TEMP
co -r$revlat  /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST

echo "<BR>"
echo "<BR>"

# Now check that input daye is valid
if (test $inputdate -lt "19990920") 
then
# Date entered is incorrect
   echo "invalid date entered"
else

   if ($inputdate -gt 99999999)
   then
# Date enterd is incorrect
      echo "Invalid date entered, please re-enter"
   else

# Date entered is in correct format
# Now echo the results to the screen (Syntax updated Apr 07 V2)
      if $(cmp -s /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST
      /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST.TEMP)
      then
         echo "No differences found"
      else
         echo "<PRE>"
         rcsdiff -C $context -r$revtest -r$revlat /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST
         echo "</PRE>"
      fi

   fi

fi
   

# Remove temp file
rm /home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST.TEMP

# Now finish of html for results page.
echo "<BR>"
echo "<HR>"
echo "If any problems then please email <A HREF='MAILTO:metdb@metoffice.gov.uk'> the MetDB team </A>"

echo "</BODY>"
echo "</HTML>"
