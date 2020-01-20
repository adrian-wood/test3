#!/bin/bash

#-----------------------------------------------------------------------
#
# SCRIPT        : config_utils.sh
#
# PURPOSE       : Collection of utility functions for parsing a config file
#                 in bash.
#
# CALLED BY     : sourced by send_data and get_data
#
# ENVIRONMENT   : CONFIG must be set to the config file.
#                 (This is not checked here)
#
# REVISION INFO :
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2019 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------


get_config() {

# Parse the config file to get the value of a specified variable.
# Argument:
#    (1) name of the variable
#
# usage:
#    subtype=$(get_config "subtype")

# (gets the line starting with the arg | gets the bit after the equals | trims white space)
# Echoing the result is a way of getting text back to the caller.

  var=$(grep ^"$1" "$CONFIG" | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')
# expand any variables in the string
  expvar=$(echo "$var" | envsubst)
  echo "$expvar"

}

check_config() {

# Check a variable has been set.
# Arguments:
#    (1) name of the variable to be checked
#    (2) value of the variable
#
# usage:
#    check_config "subtype" $subtype

  if [ -z "$2" ]; then
    echo "$1 is undefined in $CONFIG"
    exit 1
  fi
}

