#!/usr/bin/perl -w
# Monitor to display results of RPC tidy-up jobs which are run daily on GPCS.
# Pages are archived.
# ===========================================================================
# written by Sheila Needham 3 March 2003
# updated 21 Jun 2007 to add output from BPRD jobs SMN
# ===========================================================================

# initialise variables
&initialise;

# get files from GPCS
&get_data;

# Format html page
&page_out;

# Archive page
&archive_page;
      
exit 0;
 
# ============  Subroutines ================================================
sub initialise {
# initialise variables

$HOME="/net/home/h01/usmdb/public_html/rpc/monitor";
($dum,$dum,$dum,$day,$month,$year,$dum,$dum,$dum)=localtime;
$month= ++$month;
$year= $year + 1900;
$URL="http://www-metdb-test/~usmdb/rpc/monitor/archive";
$runtime=scalar(localtime);

}
# ===========================================================================
sub get_data {

# ftp output file from GPCS
my @args = ("ftp ukmet << eof
cd ..
get MDB.RPCANCEL.OUTPUT $HOME/output.txt
get /u/os/t12db/rpc_stats/reset.out $HOME/output2.txt
get MDB.RPCANCEL.OUTPUTB $HOME/output3.txt
quit
eof");

system(@args) == 0 or die "system @args failed: $?";

# ftp output from BPRD
my @args2 = ("ftp  bprd << eof
cd ..
get /u/os/t12db/rpc_stats/resetg.out $HOME/output4.txt
quit
eof");

system(@args2) == 0 or die "system @args failed: $?";


# open data files 
 
open(FILE2,"<$HOME/output.txt") or die "Cannot open output.txt";
open(FILE3,"<$HOME/output2.txt") or die "Cannot open output2.txt";
open(FILE4,"<$HOME/output3.txt") or die "Cannot open output3.txt";
open(FILE5,"<$HOME/output4.txt") or die "Cannot open output4.txt";

}
# =========================================================================
sub page_out {

#file for html output

my $filename=$HOME."/output.html";
open (FILE1,">$filename") or die "Cannot open $filename";

# write header

print FILE1 "<html>\n";
print FILE1 "<head>\n";
print FILE1 "<title>RPC server deletion monitor</title>\n";
print FILE1 "</head>\n";
print FILE1 "<body bgcolor=#FFFFFF text=#000000 link=#0000FF vlink=#800080>\n";
print FILE1 "Monitor ran on $runtime";
print FILE1 "<br /><br /><p>Output from EJES clear up script on PROD</p>";
# write data files into html page

print FILE1 "<pre>";
while (<FILE2>) {
  print FILE1 "$_";
}
print FILE1 "</pre>";
print FILE1 "<br /><p>Output from reset_prognum on PROD</p>";

print FILE1 "<pre>";
while (<FILE3>) {
  print FILE1 "$_";
}
print FILE1 "</pre>";

print FILE1 "<br/><br/><p>Output from EJES clear up script on BPRD</p>";

print FILE1 "<pre>";
while (<FILE4>) {
  print FILE1 "$_";
}
print FILE1 "</pre>";
print FILE1 "<br/><p>Output from reset_prognum on BPRD</p>";

print FILE1 "<pre>";
while (<FILE5>) {
  print FILE1 "$_";
}
print FILE1 "</pre>";

# write footer

print FILE1 "<br / ><br />";
print FILE1 "<A href=\"$URL\">Archive data</A><br><br>";
print FILE1 "</body></html>\n";
close (FILE1);
}

sub archive_page {

# archive today's page

chdir ($HOME) ;
chdir ("archive");
 
# check if directory exists else create

if (!(-e $year)) {
  print "No year directory, $year\n";
  mkdir ($year,0755);
  }

chdir ($year); 

if (! (-e $month)) {
  print "No month directory, $month\n";
  mkdir ($month,0755);
  }
 
chdir ($month);

#copy page to archive

`cp $HOME/output.html ./archive_$day$month$year.html`

}
