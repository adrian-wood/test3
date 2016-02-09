#!/bin/bash                                                                      
#------------------------------------------------------------------------        
# PURPOSE : Script to run wfsyn program to retrieve latest LNDSYN data           
#           for Heathrow and encode it as BUFR using Windfarm sequence.
#           Run hourly at hh:15.
#------------------------------------------------------------------------

echo "$0: starting at `date`"
cd ~usmdb/WFSYN

# Required MetDB environment variables
export METDB_SERVER_NUMBER=33556618
export METDB_FREEPN_NUMBER=33556619
export METDB_SERVER_IPNAME=mdb-test
export METDB_CLIENT_CONTACT=andrew.moorhouse@metoffice.gov.uk

# Optional MetDB environment variables
export METDB_RPC_TIMEOUT=5400
export METDB_DEBUG_LEVEL=0
# export METDB_TEMPFILE1=tempfile
# export METDB_VERSION=x.y.z

# Required BUFR Encoding variables
export BUFR_LIBRARY=./MetDB_BUFR24.0.00/tables/

# Remove the existing file
rm -f ./WFSYN.bufr > /dev/null 2>&1

echo "$0:executing wfsyn.exe..."
./wfsyn.exe

if [ $? -eq 0 ]; then
    echo "$0: program wfsyn ended with return code 0; moving WFSYN.bufr into place"
    # Amend following lineis to move or ftp etc WFSYN.bufr to wherever it will be processed...
    export INCOMING=/tmp
    #export INCOMING=/var/moods/bulletins/incoming/BUF1
    export FILENAME=MHSR.R1D`date +%y%j.T%H%M%S`.BUF1.S000
    mv ./WFSYN.bufr  $INCOMING
    mv $INCOMING/WFSYN.bufr $INCOMING/$FILENAME
    if [ $? -ne 0 ]; then
        echo "$0: Error putting WFSYN.bufr into place"
    fi
fi

echo "$0: ended at `date`"
exit

