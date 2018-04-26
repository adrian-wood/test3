# ----------------------------------------------------------------
# Unix Script to get MetDB source from operational libraries
# once a week, and run it through f2html to produce html copies for
# browsing on Netscape. 
# -----------------------------------------------------------------

cd /net/home/h01/usmdb/GetMdbSource/WorkDir

# -------------------------------------------------------------
# Step 1: cp source from operational libraries.
# -------------------------------------------------------------

cp /net/home/h01/mdb_new/op/lib/source/*.[fF] .
cp /net/home/h01/mdb_new/op/lib/merge/*.[fF] .

# ------------------------------------------------------------------
# Step 2: run f2html 
# ------------------------------------------------------------------

$HOME/bin/f2html *.[fF]
cat f2html_index.html $HOME/bin/ind_add > index.html

# ------------------------------------------------------------------
# Step 3: delete *.F files and cp *.html files.  
# ------------------------------------------------------------------
  
chmod 777 *.[fF]

mv *.html /net/home/h01/usmdb/public_html/source_listings/mdb/source

rm *.[fF] 

# ------------------------------------------------------------------
# Step 4: announce end of script execution
# ------------------------------------------------------------------

echo
echo 'script GetMdbSource ended'
echo

exit
