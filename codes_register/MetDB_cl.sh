#! /bin/sh
#
#
################################################################################################################
#
# Program:          MetDB_cl.sh
#
# Date:             18/02/2018
#
# Purpose:          Intiate and run MetDB_Code_listing.py. This will update 
#                   the MetDB codes register.
#
#
################################################################################################################            
#
#
# create the mdb-repo name with the date timestamp appended to the end
#
mdrp="/tmp/metdb-repo"
mdt=$(date +"%Y%m%d_%H%M%S")
expd=$mdrp$mdt
#
# create git command to clone repository
#
GIT_CMD="git clone ssh://git@exxgitrepo:7999/mood/metdb.git "$expd
#
$GIT_CMD
#
# run python script with env variable $MDPRF as input parameter
#
python MetDB_Code_listingt1.py $expd
