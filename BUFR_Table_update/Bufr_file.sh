#! /bin/sh
#
#
##############################################################################################################################################################################################
#
#                   Program Name:	ret_file.sh
#
#		    Author:             Richard Weedon
#
#                   Date:               3rd February 2016                
#
#                   Purpose:            Build the MetDB BUFR Tables for use with the BUFR Package and / or MetDB BUFR decoder. 
#
#                                       The current versions of the MetDB BUFR files will be extracted from MDBDB-PREPROD using scp , whilst the latest WMO files are extracted 
#                                       from the zip file downloaded from the WMO Codes Page.
#
#                                       The shell script will execute in four stages :-
#
#                                       1. Establish default values for all variables and display the Menu. 
#
#                                       2. Regardless of the option selected the next step will execute. Request the URL for the BUFR zip file from the WMO Codes page. The naming convention
#                                          for the zip file will include the version number which will be used to create the root of the build directory structure.
#
#                                          Create the build directorys, using the version number of the latest WMO BUFR issue as part of the naming convention. For version 25 of the 
#                                          BUFR Table release the naming convention will be of the form ~/BUFRCREX_25_0_0  from which the following directory structure will be built-
#                                            
#                                          ~/BUFRCREX_25_0_0                (root )
#                                          ~/BUFRCREX_25_0_0/op             (MetDB operational BUFR tables B, C & D)
#                                          ~/BUFRCREX_25_0_0/Bufr_b         (BUFR Table B - WMO Version 25  )                                  
#                                          ~/BUFRCREX_25_0_0/Bufr_d         (BUFR Table D - WMO Version 25  ) 
#                                          ~/BUFRCREX_25_0_0/Bufr_c         (BUFR Table C - WMO Version 25  ) 
#                                          ~/BUFRCREX_25_0_0/common_ct      (BUFR common code tables - WMO Version 25  )
#                                          ~/BUFRCREX_25_0_0/output         BUFR and Codetables output                      
#                                         
#                                       3. The measures taken in the next step will be determined by the option selected, but in general will entail the extraction of the text versions 
#                                          of Tables A , B & D from the WMO Zip file to the directories outlined above. Where option 5 has been selected, the url of the common code tables
#                                          zip file will be requested from which common code tables 1,2,5,7 & 8 will be extracted. Finally option 5 will entail the extraction of the 
#                                          CodeFlag table from the BUFR zip file. The MetDB BUFR tables will be copied from MDBDB-PREPROD depending on the option selected.                                           
#
#                                       4. Finally the variables established from the steps above will be passed to BUFR Tables.py as an array. 
#
##############################################################################################################################################################################################
#
#
# 1. Establish default values for all variables and display the Menu
#
one="1"
two="2"
three="3"
four="4"
five="5"
six="6"
seven="7"
eight="8"
nine="9"
ten="10"
fpat="*.zip"
No="N"
opt1="NULL"
sdir="NULL"
op_fileb="NULL"
op_filed="NULL"
op_filec="NULL"
op_fileCF1="NULL"
op_fileC1="NULL"
op_fileC2="NULL"
op_fileC5="NULL"
op_fileC7="NULL"
op_fileC8="NULL"
lc_fileb="NULL"
lc_filed="NULL"
lc_cfig="NULL"
opb_file="bufr_tableb"
opd_file="bufr_tabled"
opct_file="bufr_codefig"
seq_id="NULL"
rm retrieval.txt
rm ret2.txt
rm ret_BUFR.txt
rm ret_CCT.txt
show_menu(){
echo -e "*****************************************************************************************************"
echo -e " "
echo -e " Please Select from the following "
echo -e " "
echo -e "1) Update MetDB (Operational) version of BUFR Table B with New local entry "
echo -e " "
echo -e "2) Update MetDB (Operational) version of BUFR Table D with New local entry"
echo -e " "
echo -e "3) Update MetDB (Operational) version of BUFR Table B from the latest WMO version  "
echo -e " "
echo -e "4) Update MetDB (Operational) version of BUFR Table D from the latest WMO version  "
echo -e " "
echo -e "5) Update MetDB (Operational) version of Codefigs from the latest WMO version  "
echo -e " "
echo -e "6) Compare the Operational (MetDB) and WMO versions of BUFR Table B "
echo -e " "
echo -e "7) Compare the Operational (MetDB) and WMO versions of BUFR Table D "
echo -e " "
echo -e "8) Expand a WMO Table D Sequence to its constituent B descriptors "
echo -e " "
echo -e "9) Expand a WMO Table D Sequence and produce an outline of the MetDB element index "
echo -e " "
echo -e "10) Exit the application "
echo -e " "
echo -e "Note options 1 - 4 & 6 - 9 will use wget to extract the latest versions of the BUFR and / or "
echo -e "Codefig tables from the WMO Code Tables Web Site. In addition the current operational versions "
echo -e "of the BUFR and Codefig tables will be copied (scp) from MDBDB-PREPROD. To complete this "
echo -e "the user must have access to the moodsf account on the MetDB. "
echo -e " "
echo -e "*****************************************************************************************************"
read opt1
}
show_menu
opt2=$opt1
#
# 2. Request the url of the BUFR zip file on the WMO Codes web site. Extract the latest BUFR version and  build 
#    the directory structure 
# 
if [ "$opt1" != "$ten" ]; then
   read -p "URL of the BUFR Zip file from the WMO Codes site: " locat
   IFS='/' read -ra dname<<<"$locat"   
   dname2=${dname[9]}                                                   # name of the BUFR zip file
   IFS='/' read -ra dname3<<<"$dname2"
   IFS='.' read -ra dname4<<<"$dname3" 
   dname5=${dname4[0]}
   dname4=${dname3[0]}
   sdir=$HOME"/"$dname5
   zfdir=$sdir"/"$dname5".zip"
   sdir2=$sdir"/*.zip"
   fileB=$sdir/ret_BUFR.txt
   fileC=$sdir/ret_CCT.txt
   echo $fileB
   echo $fileC
    
   if [ -d "$sdir" ]; then                                                                                    # check dir structure
      echo " Directory structure (including source files) has already been established. "
      read -p "This may contain older versions of the source files, do you wish to proceed? Y/N: " res
      if [ "$res" == "$No" ]; then
         echo "Before proceeding the existing directory tree and its contents must be deleted "
         exit
      else
         echo "Proceeding with the existing directory structure $Sdir and source files"
         if [ -e $fileB ]; then
            rm "$fileB"
            echo "$fileB removed "
         fi
         if [ -e $fileC ]; then
            rm "$fileC"
            echo "$fileC removed "
         fi 
      fi
   else
      mkdir $sdir
      mkdir $sdir/BUFR_b
      mkdir $sdir/BUFR_d
      mkdir $sdir/BUFR_c
      mkdir $sdir/common_ct
      mkdir $sdir/op
      mkdir $sdir/output
   fi
else
   echo "Terminating the procedure............"
   exit
fi
#
# 3. Extract the tables from the BUFR zip file and for option 5 repeat the step for the common codes table.
#    SCP the MetDB BUFR files from MDBDB-PREPROD
#
if [ -d "$sdir" ]; then  
   if [ -e $zfdir ]; then
      echo "$dname4 is already present, will proceed with the existing file "
      zipinfo -1 $zfdir >> $fileB 
   else
      wget $locat -P $sdir   
      zipinfo -1 $zfdir >> $fileB   
   fi
   if [ "$opt1" == "$five" ]; then                                                # Option five requires access to the WMO common Codes zip file 
      read -p "URL of the Common Code Tables Zip file from the WMO Codes site: " locatct
      IFS='/' read -ra dnamect<<<"$locatct"   
      dname2ct=${dnamect[9]}                                                      # name of the common code tables zip file                       
      IFS='/' read -ra dname3ct<<<"$dname2ct"
      IFS='.' read -ra dname4ct<<<"$dname3ct"      
      dname5ct=${dname4ct[0]}
      dname4ct=${dname3ct[0]}
      sdirct=$HOME"/"$dname5"/common_ct/"
      zfdirct=$sdirct"/"$dname5ct".zip"
      sdir2ct=$sdirct"/*.zip"
      if [ -d "$sdirct" ]; then  
         if [ -e $zfdirct ]; then
            echo "$dname4ct is already present, will proceed with the existing file "
            zipinfo -1 $zfdirct >> $fileC
         else
            wget $locatct -P $sdirct 
            zipinfo -1 $zfdirct >> $fileC
         fi
      else
         echo "Directory structure incorrect "
         echo "Terminating the procedure............"
         exit
      fi
   fi
fi
#

while read line                                                                                                # spool through listing of BUFR zip file. Output results to array files2
do
   files2+=("$line")
done <$fileB
#
#
if [ "$opt1" == "$one" ] || [ "$opt1" == "$three" ] || [ "$opt1" == "$six" ]; then    
   for file in "${files2[@]}"                                                                                      # spool through the files2 array unzip the files needed. 
   do
      if echo "$file" | grep -q "TableB_en.txt" ;
      then
         if echo "$file" | grep -q "BUFRCREX_";
         then
             IFS='/' read -ra op_temp<<<"$file"
             op_temp2=${op_temp[1]} 
             op_fileb="$sdir/BUFR_b/$op_temp2"
             if [ -e "$op_fileb" ]; then  
                echo "$op_fileb already exists will use the existing copy"
             else
                echo "unzipping $file "
                unzip -ju $zfdir $file -d $sdir/BUFR_b
             fi
         fi
      else
         echo "$file not required "
      fi
   done  
   lc_fileb=$sdir/op/$opb_file
   if [ -e "$lc_fileb" ]; then 
      echo "$lc_fileb already exists will use the existing copy"
   else
      scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opb_file $sdir/op                                   # scp the operational version of BUFR Table B from pre-prod
   fi
fi
#
#
if [ "$opt1" == "$two" ] || [ "$opt1" == "$four" ] || [ "$opt1" == "$seven" ]; then    
   for file in "${files2[@]}"                                                                                      # spool through the files2 array unzip the files needed. In this case the WMO Versions of Table D
   do
      if echo "$file" | grep -q "TableD_en.txt" ;
      then
         if echo "$file" | grep -q "BUFR_";
         then
             IFS='/' read -ra op_temp<<<"$file"
             op_temp2=${op_temp[1]} 
             op_filed="$sdir/BUFR_d/$op_temp2"
             if [ -e "$op_filed" ]; then  
                echo "$op_filed already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdir $file -d $sdir/BUFR_d
             fi
         fi
      else
         echo "$file not required "
      fi
   done   
   lc_filed=$sdir/op/$opd_file
   if [ -e "$lc_filed" ]; then 
      echo "$lc_filed already exists will use the existing copy"
   else
      scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opd_file $sdir/op                                   # scp the operational version of BUFR Table D from pre-prod
   fi
fi
#
#
if [ "$opt1" == "$eight" ] || [ "$opt1" == "$nine" ] ; then
   if [ "$opt1" == "$eight" ]; then  
       echo "This option will expand an existing (Global) D Sequence."
       read -p " Enter FXY of the D Sequence : " seq_id
   fi
   if [ "$opt1" == "$nine" ]; then  
       echo "This option will expand an existing (Global) D Sequence and produce an outline of the retrieval element index."
       read -p " Enter FXY of the D Sequence : " seq_id
   fi
   for file in "${files2[@]}"                                                                                      # spool through the files2 array unzip the files needed. In this case the WMO Versions of Table D
   do
      if echo "$file" | grep -q "TableD_en.txt" ;
      then
         if echo "$file" | grep -q "BUFR_";
         then
             IFS='/' read -ra tabled_sp<<<"$file"   
             op_filed="$sdir/BUFR_d/${tabled_sp[1]}"
             if [ -e "$op_filed" ]; then  
                echo "$op_filed already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdir $file -d $sdir/BUFR_d
             fi 
         fi
      else
         echo "$file not required "
      fi
#   
      if echo "$file" | grep -q "TableB_en.txt" ;
      then
         if echo "$file" | grep -q "BUFRCREX_";
         then
             IFS='/' read -ra tableb_sp<<<"$file"   
             op_fileb="$sdir/BUFR_b/${tableb_sp[1]}"
             if [ -e "$op_fileb" ]; then  
                echo "$op_fileb already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdir $file -d $sdir/BUFR_b
             fi
         fi
      else
         echo "$file not required "
      fi
#
      if echo "$file" | grep -q "TableC_en.txt" ;
      then
         if echo "$file" | grep -q "BUFR_";
         then
             IFS='/' read -ra tablec_sp<<<"$file"   
             op_filec="$sdir/BUFR_c/${tablec_sp[1]}"
             if [ -e "$op_filec" ]; then  
                echo "$op_filec already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdir $file -d $sdir/BUFR_c
             fi
         fi
      else
         echo "$file not required "
      fi
   done                                     
   lc_filed=$sdir/op/$opd_file
   lc_fileb=$sdir/op/$opb_file
   if [ -e "$lc_filed" ]; then 
      echo "$lc_filed already exists will use the existing copy"
   else
      scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opd_file $sdir/op                                   # scp the operational version of BUFR Table D from pre-prod
   fi
   if [ -e "$lc_fileb" ]; then 
      echo "$lc_fileb already exists will use the existing copy"
   else
      scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opb_file $sdir/op                                   # scp the operational version of BUFR Table D from pre-prod
   fi
fi
#
if [ "$opt1" == "$five" ] ; then  
    while read line                                                                                                # spool through listing of BUFR zip file. Output results to array files2
    do
      files3+=("$line")
    done <ret_CCT.txt   

    while read line                                                                                                # spool through listing of BUFR zip file. Output results to array files2
    do
      files4+=("$line")
    done <ret_BUFR.txt  

    for file in "${files4[@]}"                                                                                    # spool through the files3 array unzip the files needed. In this case the WMO Versions Common
    do  
      if echo "$file" | grep -q "CodeFlag_en.txt" ;
      then
         if echo "$file" | grep -q "BUFRCREX";
         then
             IFS='/' read -ra cct_table<<<"$file"   
             cct_tablef=${cct_table[1]}   
             op_fileCF1="$sdir/common_ct/$cct_tablef"
             if [ -e "$op_fileCF1" ]; then  
                echo "$op_fileCF1 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdir $file -d $sdir/common_ct
             fi
         fi
      fi
    done
    for file in "${files3[@]}"                                                                                    # spool through the files3 array unzip the files needed. In this case the WMO Versions Common
    do                                                                                                            # code tables
      if echo "$file" | grep -q "en.txt" ;
      then
         if echo "$file" | grep -q "Common_C01";
         then
             op_fileC1="$sdir/common_ct/$file"
             if [ -e "$op_fileC1" ]; then  
                echo "$op_fileC1 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdirct $file -d $sdir/common_ct
             fi
         fi
         if echo "$file" | grep -q "Common_C02";
         then
             op_fileC2="$sdir/common_ct/$file"
             if [ -e "$op_fileC2" ]; then  
                echo "$op_fileC2 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdirct $file -d $sdir/common_ct
             fi
         fi
         if echo "$file" | grep -q "Common_C05";
         then
             op_fileC5="$sdir/common_ct/$file"
             if [ -e "$op_fileC5" ]; then  
                echo "$op_fileC5 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdirct $file -d $sdir/common_ct
             fi
         fi
         if echo "$file" | grep -q "Common_C07";
         then
             op_fileC7="$sdir/common_ct/$file"
             if [ -e "$op_fileC7" ]; then  
                echo "$op_fileC7 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdirct $file -d $sdir/common_ct
             fi
         fi
         if echo "$file" | grep -q "Common_C08";
         then
             op_fileC8="$sdir/common_ct/$file"
             if [ -e "$op_fileC8" ]; then  
                echo "$op_fileC8 already exists will use the existing copy"
             else
                echo "unzipping $file"
                unzip -ju $zfdirct $file -d $sdir/common_ct
             fi
         fi
      else
         echo "$file not required "
      fi
    done  
    lc_cfig=$sdir/op/$opct_file
    if [ -e "$lc_cfig" ]; then 
      echo "$lc_cfig already exists will use the existing copy"
   else
      scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opct_file $sdir/op                                   # scp the operational version of BUFR codefig from pre-prod
   fi
fi
#
# 4. Populate the final array and submit to the python script BUFR_Tables.py.
# 
final_arr+=($opt1)
final_arr+=($sdir)
final_arr+=($op_fileb)
final_arr+=($op_filed)
final_arr+=($op_filec)
final_arr+=($op_fileCF1)
final_arr+=($op_fileC1)
final_arr+=($op_fileC2)
final_arr+=($op_fileC5)
final_arr+=($op_fileC7)
final_arr+=($op_fileC8)
final_arr+=($lc_fileb)
final_arr+=($lc_filed)
final_arr+=($lc_cfig)
final_arr+=($seq_id)
python BUFR_Tables.py ${final_arr[@]}
#
