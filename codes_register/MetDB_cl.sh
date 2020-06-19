#! /bin/sh
#
#
################################################################################################################
#
# Program:          MetDB_cl.sh
#
# Date:             14 Aug 2019
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
mdrp="/var/tmp/metdb-repo-"
mdt=$(date +"%Y%m%d_%H%M%S")
expd=$mdrp$mdt
#
expdout="http://www-mdb-apps-test/code_dep/output/"
expdin="/var/www/html/code_dep/output/"
link_files="/var/moods/code_register/template_ref/"  
#
# create git command to clone repository
#
GIT_CMD="git clone ssh://git@exxgitrepo:7999/mood/metdb.git "$expd
#
$GIT_CMD
#
#
python MetDB_Code_listing.py $expd $expdout $link_files $expdin
#
