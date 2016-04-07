#!/bin/bash

# Set up the RPC environment 
if [[ $# -ne 1 ]]
then
  echo "$0 needs the name of the system as an argument"
  exit 1
fi

sys=$1

METDB_SERVER_NUMBER=33556618
METDB_FREEPN_NUMBER=33556619
METDB_RPC_TIMEOUT=5400
METDB_DEBUG_LEVEL=1
METDB_LINUX_RPC=1
METDB_CLIENT_CONTACT=sheila.needham@metoffice.gov.uk
METDB_SERVER_IPNAME=$sys

export METDB_SERVER_NUMBER
export METDB_FREEPN_NUMBER
export METDB_RPC_TIMEOUT
export METDB_DEBUG_LEVEL
export METDB_LINUX_RPC
export METDB_SERVER_IPNAME
export METDB_CLIENT_CONTACT

