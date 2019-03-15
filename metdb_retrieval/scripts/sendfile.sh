#!/bin/sh
#-----------------------------------------------------------------------
#
# SCRIPT        :sendfile.sh
#
# PURPOSE       : function to send a file to a given location via
#                 FTP and check it is successful.
#
# ARGUMENTS     : 1 - server name
#                 2 - input file name
#                 3 - output file directory
#                 4 - output file name
#
# RETURNS       : 0 - successful
#                 1 - error
#
# REVISION INFO :
#
# MB-1790: First version.                                        Sheila Needham
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2018 MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------

sendfile(){

  SERVER=$1
  INFILE=$2
  DESTDIR=$3
  OUTFILE=$4
  echo "copying $INFILE to $DESTDIR/$OUTFILE on $SERVER"

  ftplog=$(mktemp /tmp/ftplog.XXXXXXXX)

ftp -v $SERVER <<EOF > $ftplog
binary
cd $DESTDIR/
put $INFILE $OUTFILE.tmp
rename $OUTFILE.tmp $OUTFILE
bye
EOF

  rc=$(grep "Rename successful" $ftplog)
  if [ "$rc" = "250 Rename successful." ]
  then
    rm $ftplog
    return 0
  else
    echo "FTP transfer failed"
    cat $ftplog
    return 1
  fi
}

