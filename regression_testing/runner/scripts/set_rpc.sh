#!/bin/bash

# Set up the RPC environment 
if [[ $# -ne 2 ]]
then
  echo "$0 needs the METDB_SERVER_IPNAME and version as arguments"
  exit 1
fi

export METDB_SERVER_IPNAME=$1

export METDB_VERSION=$2


export METDB_SERVER_NUMBER=33556618
export METDB_FREEPN_NUMBER=33556619
export METDB_RPC_TIMEOUT=5400
export METDB_DEBUG_LEVEL=0
export METDB_CLIENT_CONTACT=sheila.needham@metoffice.gov.uk


