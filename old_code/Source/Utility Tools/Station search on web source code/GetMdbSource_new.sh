# ----------------------------------------------------------------
# Unix Script to get MetDB source from operational libraries
# once a week, and run it through f2html to produce html copies for
# browsing on Netscape.
# -------------------------------------------------------------
# Step 1: cp source from operational libraries.
# -------------------------------------------------------------
cd /PROD/u/os/t12db/lib/source
cp /PROD/u/os/t12db/dbopsrce/*.f90 .
cp /PROD/u/os/t12db/dbopsrce/*.F90 .
# ------------------------------------------------------------------
# Step 2: run f2htm_new.pl
# ------------------------------------------------------------------
cd /PROD/u/os/t12db/lib/source
perl /PROD/u/os/t12db/lib/f2html_new.pl *.f90 *.F90
# ------------------------------------------------------------------
# Step 3: Delete *.F files from source directory.
#
# Step 4: Delete *.html files from web directory
#
# Step 6: Delete check_uid_f90.html from output directory
#
# Step 7: Copy new *.html files to web directory
# ------------------------------------------------------------------
rm /PROD/u/os/t12db/lib/source/*.f90
rm /PROD/u/os/t12db/lib/source/*.F90
rm /PROD/u/os/t12db/lib/source/check_uid_f90.html
rm /PROD/u/os/t12db/lib/source/check_auth_f90.html
rm /usr/lpp/internet/MetoRoot/WWW01/metdb/test_listing/*.html
cp *.html /usr/lpp/internet/MetoRoot/WWW01/metdb/test_listing/
# ------------------------------------------------------------------
# Step 4: announce end of script execution
# ------------------------------------------------------------------
echo
echo 'script GetMdbSource ended'
echo
exit
