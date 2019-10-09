#! /bin/sh
#
#
##############################################################################################################################################################################################
#
#                   Program Name:	BUFR_Tables_util.sh 
#
#		    Author:             Richard Weedon
#
#                   Date:               4th Jan 2017 
#
#                   Calls               BUFR_Tab.py    
#                                       
#                                           
#                   Code:               BUFR_Tables_util.sh   BUFR_Tab.py
#
#                   Purpose:            Build the MetDB BUFR Tables for use with the BUFR Package and / or MetDB BUFR decoder. 
#
#                                       The current versions of the MetDB BUFR files will be extracted from MDBDB-PREPROD using scp , whilst the latest WMO files are extracted 
#                                       from the zip file downloaded from the WMO Codes Page.
#
#                                       The shell script will execute in n stages :-
#
#                                       1. Display the Menu and set default values. 
#
#                                       2. Regardless of which option is selected a request the name and location of the BUFR zip file dowloaded from the WMO Codes page, will be issued. The naming convention
#                                          of the zip file will provide elements needed in the naming of the root of the build directory structure.
#
#                                          The name and location of the common codes zip file downloaded from the WMO codes site will also be requested.
#         
#                                          The user will be prompted to either nominate an existing build directory, or to create a new one based upon the naming convention of the BUFR Codes zip file 
#                                          file downloaded from the WMO codes pages.  
#
#                                          Check for the existence of the the build directorys, using the version number of the latest WMO BUFR issue as part of the naming convention. 
#                                          For version 25 of the BUFR Table release the naming convention will be of the form ~/BUFRCREX_25_0_0  from which the following directory 
#                                          structure will be built and populated-
#                                            
#                                          ~/BUFRCREX_25_0_0                (root )
#                                          ~/BUFRCREX_25_0_0/op             (MetDB operational BUFR tables B, C & D)
#                                          ~/BUFRCREX_25_0_0/Bufr_b         (BUFR Table B - WMO Version 25  )                                  
#                                          ~/BUFRCREX_25_0_0/Bufr_d         (BUFR Table D - WMO Version 25  ) 
#                                          ~/BUFRCREX_25_0_0/Bufr_c         (BUFR Table C - WMO Version 25  ) 
#                                          ~/BUFRCREX_25_0_0/common_ct      (BUFR common code tables - WMO Version 25  )
#                                          ~/BUFRCREX_25_0_0/output         BUFR and Codetables output                      
#                                                                                   
#
#                                       4. Finally the variables established from the steps above will be passed to BUFR Tables.py as an array.
#
#                   Calls               BUFR_Tables.py
#
#                   Purpose:            Will read in the MetDB Operational BUFR files (B / D & Codefigs) as well as the WMO versions of the same. Will then output updated versions 
#                                       of/home/richard.weedon/test_code/Common_20161102.zip      
#
#                                 
#                                       
#
##############################################################################################################################################################################################
#
#
##############################################################################################################################################################################################
#
function check_and_build_BD () {
# establish variables
# variables for the dir structure
#
 No="N"
 Yes="Y"
 opb_file="bufr_tableb"
 opd_file="bufr_tabled"
 opct_file="bufr_codefig"
 ninen="99"
 
 echo "==================================================================================================================================="
 echo " "   
 echo "Updates to and / or the creation of new versions of the BUFR and Code tables will be built to a version controlled "
 echo "directory structure ( See section 2.0 of Tech Note 22)" 
 echo " " 
 echo "Will an existing build directory be used (../BUFRCREX_nn_0_0), or will a new version of the build directories " 
 echo "be constructed? "  
 echo " " 
 echo "==================================================================================================================================="
 echo " " 
 read -p "N - Create a new version of the build directory , Y - Use an existing build directory " response1
 echo " " 
 echo "==================================================================================================================================="
 if [ "$response1" == "$No" ]; then                                                                        # N - Take build directory name from the title of the .zip file 
    read -p "Name and location of the BUFR Zip file copied from the WMO Codes site: " locat 
    read -p "Name and location of the Common Codes Zip file copied from the WMO Codes site: " locatct      
    bf=$(echo $locat | rev | cut -d'/' -f 1 | rev)                                                         # seperate name of BUFRCREX zip file from directory path
    bc=$(echo $bf | rev | cut -d'.' -f 2 | rev)                                                            # remove file extension (.zip) from filename from BUFRCREX zip file ie BUFRCREX_27_0_0
    cf=$(echo $locatct | rev | cut -d'/' -f 1 | rev)                                                       # name of ther common code tables zip file 
    bdir=$HOME"/"$bc                                                                                       # name of new Build Directory root 
 else                                                                                                      # else use an exissting build directory
    read -p "Path of the existing build directory: " bdir
    if [ ! -d $bdir ]; then 
       echo "unable to locate Build Dir $bdir "
       echo "Terminating Procedure "
       exit
    fi
 fi 
 bufb=$bdir"/BUFR_b/" 
 bufc=$bdir"/BUFR_c/"  
 bufd=$bdir"/BUFR_d/"
 bufcc=$bdir"/common_ct/"
 bufop=$bdir"/op/"
 bufout=$bdir"/output/"   
 metdb_bufb=$bdir"/op/bufr_tableb" 
 metdb_bufc=$bdir"/op/bufr_tablec" 
 metdb_bufd=$bdir"/op/bufr_tabled" 
 metdb_bufcf=$bdir"/op/bufr_codefig" 
 fileB=$bdir/ret_BUFR.txt
 fileC=$bdir/ret_CCT.txt 
 if [ "$response1" == "$No" ]; then                                                                        # construct new build directory                           
    mkdir $bdir
    mkdir $bufb
    mkdir $bufc
    mkdir $bufd         
    mkdir $bufcc
    mkdir $bufop
    mkdir $bufout
    cp $locat $bdir
    cp $locatct $bdir
    zfdir="$bdir/$bf"
    zfdircf="$bdir/$cf" 
    echo "Populating build directorys with MetDB versions of code tables "
    scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opb_file  $metdb_bufb                         # scp the operational version of BUFR Table B from pre-prod  
    scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opd_file  $metdb_bufd                         # scp the operational version of BUFR Table D from pre-prod  
    scp moodsf@mdbdb-preprod:/usr/local/moods/latest/tables/$opct_file  $metdb_bufcf                       # scp the operational version of BUFR Common Code tables from pre-prod  
    if [ -e $zfdir ]; then                                                                         
         echo "BUFR Zip file found ( $zfdir ), extracting file listing to $fileB"
         zipinfo -1 $zfdir >> $fileB
    else
         echo "unable to locate BUFR zip file in $bdir "
         echo "Terminating Procedure "
         exit
    fi
    if [ -e $zfdircf ]; then                                                                         
         echo "BUFR Common Code Tables Zip file found ( $zfdircf) , extracting file listing to $fileC"
         zipinfo -1 $zfdircf >> $fileC
    else
         echo "unable to locate Common Code Tables zip file in $bdir "
         echo "Terminating Procedure "
         exit
    fi
#
############################################################################################################################################################################################################
#
#
    echo "Populating build directorys with WMO versions of code tables "
    while read line                                                                                        # spool through listing of BUFR zip file. Output results to array files2
    do
       files2+=("$line")
    done <$fileB 
    while read line                                                                                        # spool through listing of Common Codes Table  zip file. 
    do                                                                                                     # Output results to array files3
       files3+=("$line")
    done <$fileC
    for file in "${files2[@]}"                                                                             # spool through the files2 array unzip the files needed. 
    do
        if echo "$file" | grep -q "TableB_en.txt" ;
        then
           if echo "$file" | grep -q "BUFRCREX_" ;
           then
               IFS='/' read -ra op_tempb<<<"$file"
               op_tempb2=${op_tempb[1]} 
               op_fileb="$bufb/$op_tempb2"
               if [ -e "$op_fileb" ]; then    
                   echo "$op_fileb already exists will use the existing copy"
               else  
                   unzip -ju $zfdir $file -d $bufb
               fi 
           fi
        elif echo "$file" | grep -q "TableD_en.txt" ;
        then
           if echo "$file" | grep -q "BUFR_" ;
           then
               IFS='/' read -ra op_tempd<<<"$file"
               op_tempd2=${op_tempd[1]} 
               op_filed="$bufd/$op_tempd2"
               if [ -e "$op_filed" ]; then    
                  echo "$op_filed already exists will use the existing copy"
               else  
                  unzip -ju $zfdir $file -d $bufd
               fi 
           fi
        elif echo "$file" | grep -q "TableC_en.txt" ;
        then
           if echo "$file" | grep -q "BUFR_" ;
           then
               IFS='/' read -ra op_tempc<<<"$file"
               op_tempc2=${op_tempc[1]} 
               op_filec="$bufc/$op_tempc2"
               echo "op_filec ",$op_filec   
               echo "bufc ",$bufc   
               if [ -e "$op_filec" ]; then    
                  echo "$op_filec already exists will use the existing copy"
               else  
                  unzip -ju $zfdir $file -d $bufc
               fi 
           fi
        elif echo "$file" | grep -q "CodeFlag_en.txt" ;
        then
           if echo "$file" | grep -q "BUFRCREX" ;
           then
               IFS='/' read -ra op_tempcf<<<"$file"
               op_tempcf2=${op_tempcf[1]} 
               op_filec="$bufd/$op_tempcf2"
               if [ -e "$op_filec" ]; then    
                  echo "$op_filec already exists will use the existing copy"
               else  
                  unzip -ju $zfdir $file -d $bufcc
               fi 
           fi
        else
           echo "$file not required "
        fi
    done  
#
#    
    for file in "${files3[@]}"                                                                             # spool through the files3 array unzip the files needed. 
    do                                                                                                     # In this case the WMO Versions Common code tables
        if echo "$file" | grep -q "en.txt" ;
         then
            if echo "$file" | grep -q "Common_C01";
            then
                IFS='/' read -ra op_fileCF1<<<"$file"
                op_fileCF12=${op_fileCF1[1]}  
                op_fileCF1="$bufcc$op_fileCF12"
                if [ -e "$op_fileCF1" ]; then  
                   echo "$op_fileCF1 already exists will use the existing copy"
                else
                   echo "unzipping $file"
                   echo "file $file" 
                   echo "zfdircf $zfdircf"   
                   echo "bdir $bdir"
                   unzip -ju $zfdircf $file -d $bufcc
                fi
            fi
            if echo "$file" | grep -q "Common_C02";
            then
                IFS='/' read -ra op_fileCF2<<<"$file"
                op_fileCF22=${op_fileCF2[1]}  
                op_fileCF2="$bufcc$op_fileCF22"
                if [ -e "$op_fileCF2" ]; then  
                   echo "$op_fileCF2 already exists will use the existing copy"
                else
                   echo "unzipping $file"
                   unzip -ju $zfdircf $file -d $bufcc
                fi
            fi
            if echo "$file" | grep -q "Common_C05";
            then
                IFS='/' read -ra op_fileCF5<<<"$file"
                op_fileCF52=${op_fileCF5[1]}  
                op_fileCF5="$bufcc$op_fileCF52"
                if [ -e "$op_fileCF5" ]; then  
                   echo "$op_fileCF5 already exists will use the existing copy"
                else
                   echo "unzipping $file"
                   unzip -ju $zfdircf $file -d $bufcc   
                fi
            fi
            if echo "$file" | grep -q "Common_C07";
            then
                IFS='/' read -ra op_fileCF7<<<"$file"
                op_fileCF72=${op_fileCF7[1]}  
                op_fileCF7="$bufcc$op_fileCF72"
                if [ -e "$op_fileCF7" ]; then  
                   echo "$op_fileCF7 already exists will use the existing copy"
                else
                   echo "unzipping $file"
                   unzip -ju $zfdircf $file -d $bufcc
                fi
            fi 
            if echo "$file" | grep -q "Common_C08";
            then
                IFS='/' read -ra op_fileCF8<<<"$file"
                op_fileCF82=${op_fileCF8[1]}  
                op_fileCF8="$bufcc$op_fileCF82"
                if [ -e "$op_fileCF8" ]; then  
                   echo "$op_fileCF8 already exists will use the existing copy"
                else
                   echo "unzipping $file"
                   unzip -ju $zfdircf $file -d $bufcc  
                fi
            fi
        fi
    done
 else                                                                                                      # answer = "Y" check existing build dir for files  
    if [ "$(ls -A $bufb)" ]; then                                                                          # check if directory /BUFR_b/ is empty 
       op_fileb_n=$( ls $bufb -t1 | head -n 1 )                                                            # check for WMO bufr table B
       op_fileb=$bufb$op_fileb_n
    else
       echo "Error directory $bufb is empty "
       echo "terminating execution "
       exit
    fi
    if [ "$(ls -A $bufd)" ]; then                                                                          # check if directory /BUFR_d/ is empty                                                 
       op_filed_n=$( ls $bufd -t1 | head -n 1 )                                                            # check for WMO bufr table D
       op_filed=$bufd$op_filed_n
    else
       echo "Error directory $bufd is empty "
       echo "terminating execution "
       exit
    fi
    if [ "$(ls -A $bufc)" ]; then                                                                          # check if directory /BUFR_c/ is empty                                                 
       op_filec_n=$( ls $bufc -t1 | head -n 1 )                                                            # check for WMO bufr table C
       op_filec=$bufc$op_filec_n  
    else
       echo "Error directory $bufc is empty "
       echo "terminating execution "
       exit
    fi
    if [ "$(ls -A $bufcc)" ]; then                                                                          # check if directory /common_ct/ is empty                                                 
       defc=$( ls $bufcc -t1 | head -n 1 )                                                                  # check for WMO bufr table C
    else
       echo "Error directory $bufcc is empty "
       echo "terminating execution "
       exit
    fi
    if [ "$(ls -A $metdb_bufb)" ]; then                                                                    # check if directory /op/bufr_tableb/ is empty 
       def_metdbb=$metdb_bufb
    else
       echo "$metdb_bufb does not exist "
       echo "terminating execution "
       exit
    fi
    if [ "$(ls -A $metdb_bufd)" ]; then                                                                     # check if directory /op/bufr_tabled/ is empty                                                 
       def_metdbd=$metdb_bufd
    else
       echo "$metdb_bufd does not exist "
       echo "terminating execution "
       exit
    fi

    if [ $(ls $bdir/common_ct/Common_C01* 2> /dev/null | wc -l) ]; then                       # check if common code table directory has file matching C01                              
       op_fileCF1=$( ls -t $bdir/common_ct/Common_C01* | head -1 ) 
    else
       echo "$bdir/common_ct/Common_C01* does not exist "
       echo "terminating execution "
       exit
    fi
    if [ $(ls $bdir/common_ct/Common_C02* 2> /dev/null | wc -l) ]; then                       # check if common code table directory has file matching C02                              
       op_fileCF2=$( ls -t $bdir/common_ct/Common_C02* | head -1 ) 
    else
       echo "$bdir/common_ct/Common_C02* does not exist "
       echo "terminating execution "
       exit
    fi

    if [ $(ls $bdir/common_ct/Common_C05* 2> /dev/null | wc -l) ]; then                       # check if common code table directory has file matching C05                              
       op_fileCF5=$( ls -t $bdir/common_ct/Common_C05* | head -1 ) 
    else
       echo "$bdir/common_ct/Common_C05* does not exist "
       echo "terminating execution "
       exit
    fi

    if [ $(ls $bdir/common_ct/Common_C07* 2> /dev/null | wc -l) ]; then                       # check if common code table directory has file matching C07                              
       op_fileCF7=$( ls -t $bdir/common_ct/Common_C07* | head -1 ) 
    else
       echo "$bdir/common_ct/Common_C07* does not exist "
       echo "terminating execution "
       exit
    fi

    if [ $(ls $bdir/common_ct/Common_C08* 2> /dev/null | wc -l) ]; then                       # check if common code table directory has file matching C08                              
       op_fileCF8=$( ls -t $bdir/common_ct/Common_C08* | head -1 ) 
    else
       echo "$bdir/common_ct/Common_C08* does not exist "
       echo "terminating execution "
       exit
    fi

 fi
 echo "==================================================================================================================================="
 echo " "
 echo "The following default values have been set - "
 echo " "
 echo "            Table Name            :   Location          "
 echo "==================================================================================================================================="
 echo " "
 echo " 1. MetDB BUFR Table B            : $metdb_bufb "
 echo " 2. MetDB BUFR Table D            : $metdb_bufd "
 echo " 3. MetDB BUFR Common Code Tables : $metdb_bufcf "
 echo " 4. WMO BUFR Table B              : $op_fileb " 
 echo " 5. WMO BUFR Table D              : $op_filed "
 echo " 6. WMO BUFR Table C              : $op_filec "
 echo " 7. WMO BUFR Code Table C01       : $op_fileCF1 "
 echo " 8. WMO BUFR Code Table C02       : $op_fileCF2 " 
 echo " 9. WMO BUFR Code Table C05       : $op_fileCF5 "
 echo "10. WMO BUFR Code Table C07       : $op_fileCF7 "
 echo "11. WMO BUFR Code Table C08       : $op_fileCF8 " 
 echo " "
 echo "==================================================================================================================================="
 echo " "
 # 
 opt1="0"
 while [ $opt1 != $ninen ]
 do
   echo "Enter the No of the default value to correct or 99 to proceed." 
   read -p "=> " opt1 
   if [ $opt1 != $ninen ]; then 
      echo "Enter the new default value." 
      read -p "=> " defn
      case $opt1 in 
         "1") metdb_bufb=$defn
         ;;
         "2") metdb_bufd=$defn 
         ;;
         "3") metdb_bufcf=$defn
         ;;  
         "4") op_fileb=$defn
         ;;
         "5") op_filed=$defn
         ;;
         "6") op_filec=$defn
         ;; 
         "7") op_fileCF1=$defn
         ;;
         "8") op_fileCF2=$defn
         ;;
         "9") op_fileCF5=$defn
         ;;
         "10") op_fileCF7=$defn
         ;;
         "11") op_fileCF8=$defn
         ;;
      esac
   fi
 done
# echo "==================================================================================================================================="
echo " "    
}  # end of function
###############################################################################################################################################################################################
#
#
#
show_menu(){
echo "==================================================================================================================================="
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
echo -e "Note "
echo -e "The application requires the filename and locations of the zip files "
echo -e "containing BUFR Tables B & D and CodeFigs.   "
echo -e "   "
echo -e "NOTE: The WMO release version number is extracted from the zip filename."
echo -e " "
echo "==================================================================================================================================="
echo " "
echo "Enter Option " 
read -p "=> " optin
echo " "  
}
show_menu
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
eleven="11"
bdir="NULL"
op_fileb="NULL"
op_filed="NULL"
op_filec="NULL"
op_fileCF1="NULL"
op_fileC1="NULL" 
op_fileC2="NULL" 
op_fileC5="NULL" 
op_fileC7="NULL" 
op_fileC8="NULL"  
metdb_bufb="NULL" 
metdb_bufd="NULL" 
metdb_bufcf="NULL" 
seq_id="NULL" 
if [ "$optin" != "$ten" ]; then
  check_and_build_BD  
  if [ "$optin" == "$eight" ]; then 
     echo "Enter the D sequence to be expanded." 
     read -p "=> " seq_id 
  fi
else
  echo "exiting application ....."
  exit
fi 
#
##############################################################################################################################################################################################
#
#   Populate the final array and submit to the python script BUFR_Tables.py.
# /home/richard.weedon/test_code/Common_20161102.zip
#   $optin       =>      option indicator 
#   $bdir        =>      root of the bufr directory
#   $op_fileb    =>      WMO version of BUFR Table B (txt)
#   $op_filed    =>      WMO version of BUFR Table D (txt)
#   $op_filec    =>      WMO version of BUFR Table C (txt)
#   $op_fileCF1  =>      WMO version of BUFR code and flag tables (txt) 
#   $op_fileC1   =>      WMO version of Common Code tables No (1)
#   $op_fileC2   =>      WMO version of Common Code tables No (2)
#   $op_fileC5   =>      WMO version of Common Code tables No (5)
#   $op_fileC7   =>      WMO version of Common Code tables No (7)
#   $op_fileC8   =>      WMO version of Common Code tables No (8)
#   $lc_fileb    =>      MetDB version of BUFR Table B (txt)
#   $lc_filed    =>      MetDB version of BUFR Table D (txt)
#   $lc_cfig     =>      MetDB version of the codefig tables (txt)
#   $seq_id      =>      Table D ID 
#
##############################################################################################################################################################################################
#
  final_arr+=($optin)
  final_arr+=($bdir)
  final_arr+=($op_fileb)
  final_arr+=($op_filed)
  final_arr+=($op_filec) 
  final_arr+=($op_fileCF1)
  final_arr+=($op_fileC1)
  final_arr+=($op_fileC2)
  final_arr+=($op_fileC5)
  final_arr+=($op_fileC7)
  final_arr+=($op_fileC8)
  final_arr+=($metdb_bufb)
  final_arr+=($metdb_bufd)
  final_arr+=($metdb_bufcf)
  final_arr+=($seq_id)
  python BUFR_Tab3_int.py ${final_arr[@]}


