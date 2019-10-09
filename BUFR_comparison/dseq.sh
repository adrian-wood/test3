#! /bin/sh
#
#
################################################################################################################
#
# Program:          dseq.sh
#
# Date:             07/10/2019
#
# Purpose:          Monitor BUFR data reception for encoding sequences not represented in the 
#                   MetDB Elements index for that data type.
#
# Input:            Config table.txt          
#
# Output:           rogue_d_seq.txt
# 
#
#################################################################################################################
#
# check for presence of the output file rougue_d_seq.txt and delete if present
#
file_out="/home/moodsf/D_sequence_comparison/rogue_d_seq.txt"
if [ -f "$file_out" ]; then
     rm $file_out
     echo "$file_out removed"
fi
#
# run python script 
#
python /home/moodsf/D_sequence_comparison/rougueBUFR_Sequence_comparison.py 
