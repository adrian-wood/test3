#!/bin/bash

# Set up the RPC environment 
if [[ $# -ne 1 ]]
then
  echo "$0 needs the METDB_SERVER_IPNAME as an argument"
  exit 1
fi

sys=$1

export METDB_SERVER_NUMBER=33556618
export METDB_FREEPN_NUMBER=33556619
export METDB_RPC_TIMEOUT=5400
# export METDB_VERSION=4.37.1
export METDB_DEBUG_LEVEL=0
export METDB_CLIENT_CONTACT=sheila.needham@metoffice.gov.uk
export METDB_SERVER_IPNAME=$sys


