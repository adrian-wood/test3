#!/usr/bin/perl
#------------------------------------------------------------------
#
# ROUTINE       : webret.pl
#
# LANGUAGE      : PERL
#
# DESCRIPTION   : CGI script for MetDB web retrieval to HPMDB on
#               : us0400. Based on IBM REXX version. 
#
# CALLS         : webret.exe
#
# REVISION INFO :
#
# $Revision: 1$
# $Date: 20/09/2006 16:22:55$
# $Source: /home/us0400/mdb_new/op/lib/web_source/RCS/webret.pl,v $
#
# CHANGE RECORD :
#
# $Log:
#  1    Met_DB_Project 1.0         20/09/2006 16:22:55    Stan Kellett    
# $
# Revision 1.5  2003/02/03 13:02:12  usmdb
# Added environment variables METDB_BASE_DIR, METDB_ELEMIDX,
# METDB_STNABRV - S.Cox
#
# Revision 1.4  2001/04/06 14:25:11  usmdb
# Send stderr to $OUTFILE - S.Cox
#
# Revision 1.3  2001/03/22 11:49:36  usmdb
# set environment variable BUFR_LIBRARY for BUFR tables - S.Cox
#
# Revision 1.2  2001/01/18  10:48:06  10:48:06  usmdb (Generic MDB account)
# Various minor updates - S.Cox
# 
# Revision 1.1  2001/01/16  14:35:58  14:35:58  usmdb (Generic MDB account)
# Initial revision
# 
#
# Written by Simon Cox 16-01-2001
#------------------------------------------------------------------

print "Content-type: text/html\n\n";

my $query = getQuery();
my $input = parseQuery($query);

#------------------------------------------------------------------
# Set up environment variables for use in webret routine.
#------------------------------------------------------------------

$ENV{'SUBTYPE'}    = $input->{"SUBTYPE"};
$ENV{'SDATE'}      = $input->{"STARTDATE"};
$ENV{'SHOUR'}      = $input->{"STARTHOUR"};
$ENV{'SMINUTE'}    = $input->{"STARTMINUTE"};
$ENV{'EDATE'}      = $input->{"ENDDATE"};
$ENV{'EHOUR'}      = $input->{"ENDHOUR"};
$ENV{'EMINUTE'}    = $input->{"ENDMINUTE"};
$ENV{'SUBMIT'}     = $input->{"SUBMIT"};
$ENV{'MAXOBS'}     = $input->{"MAXOBS"};
$ENV{'HEADER'}     = $input->{"HEADER"};
$ENV{'VERSIONS'}   = $input->{"VERSIONS"};
$ENV{'INCREMENT'}  = $input->{"INCREMENT"};
$ENV{'SONDEPARTS'} = $input->{"SONDEPARTS"};
$ENV{'AREA_N'}     = $input->{"AREA_N"};
$ENV{'AREA_S'}     = $input->{"AREA_S"};
$ENV{'AREA_W'}     = $input->{"AREA_W"};
$ENV{'AREA_E'}     = $input->{"AREA_E"};

$OUTFILE               = "/var/tmp/" . $$ . "file1.txt";
$ENV{'TEMPFILE'}       = "/var/tmp/" . $$ . "file2.txt";
$ENV{'BUFR_LIBRARY'}   = "/home/us0400/mdb_new/op/data/";
$ENV{'METDB_BASE_DIR'} = "/home/us0400/mdb_new/op";
$ENV{'METDB_ELEMIDX'}  = "/home/us0400/mdb_new/op/data/element.index";
$ENV{'METDB_STNABRV'}  = "/home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST";

@stn = ($input->{"STN01"}, $input->{"STN02"}, $input->{"STN03"},
        $input->{"STN04"}, $input->{"STN05"}, $input->{"STN06"},
        $input->{"STN07"}, $input->{"STN08"}, $input->{"STN09"},
        $input->{"STN10"});

#------------------------------------------------------------------
# Initialise variables
#------------------------------------------------------------------

$lndsyn   = 0;
$shpsyn   = 0;
$ncm      = 0;
$srew     = 0;
$metars   = 0;
$tafs     = 0;
$temp     = 0;
$pilot    = 0;
$dropsond = 0;
$samosx   = 0;
$tropadv  = 0;
$aireps   = 0;
$esaws    = 0;
$bathy    = 0;
$tesac    = 0;
$bogus    = 0;
$tbus     = 0;
$amdars   = 0;
$driftr   = 0;
$buoyprof = 0;
$atafs    = 0;
$nostn    = 1;             # nostn = 1 (all stations illegal)
$get_all_stations = 0;

$lndsyn   = 1 if ($input->{"SUBTYPE"} eq "LNDSYN");
$shpsyn   = 1 if ($input->{"SUBTYPE"} eq "SHPSYN");
$ncm      = 1 if ($input->{"SUBTYPE"} eq "NCM");
$srew     = 1 if ($input->{"SUBTYPE"} eq "SREW");
$metars   = 1 if ($input->{"SUBTYPE"} eq "METARS");
$tafs     = 1 if ($input->{"SUBTYPE"} eq "TAFS");
$temp     = 1 if ($input->{"SUBTYPE"} eq "TEMP");
$pilot    = 1 if ($input->{"SUBTYPE"} eq "PILOT");
$dropsond = 1 if ($input->{"SUBTYPE"} eq "DROPSOND");
$samosx   = 1 if ($input->{"SUBTYPE"} eq "SAMOSX");
$tropadv  = 1 if ($input->{"SUBTYPE"} eq "TROPADV");
$aireps   = 1 if ($input->{"SUBTYPE"} eq "AIREPS");
$esaws    = 1 if ($input->{"SUBTYPE"} eq "ESAWS");
$bathy    = 1 if ($input->{"SUBTYPE"} eq "BATHY");
$tesac    = 1 if ($input->{"SUBTYPE"} eq "TESAC");
$bogus    = 1 if ($input->{"SUBTYPE"} eq "BOGUS");
$tbus     = 1 if ($input->{"SUBTYPE"} eq "TBUS");
$amdars   = 1 if ($input->{"SUBTYPE"} eq "AMDARS");
$driftr   = 1 if ($input->{"SUBTYPE"} eq "DRIFTR");
$buoyprof = 1 if ($input->{"SUBTYPE"} eq "BUOYPROF");
$atafs    = 1 if ($input->{"SUBTYPE"} eq "ATAFS");

#------------------------------------------------------------------
# Generate HTML header section
#------------------------------------------------------------------

print "<HTML>\n";
print "<HEAD>\n";
print '<META HTTP-EQUIV="expires" CONTENT="0">' . "\n";
print "<TITLE>WEB Linux MetDB " . $input->{"SUBTYPE"} . " Retrieval</TITLE>\n";
print "</HEAD>\n";
print "<BODY bgcolor=#ffffff>\n";
print "<CENTER>\n";
print "<H1>WEB Linux MetDB " . $input->{"SUBTYPE"} . " Retrieval </H1>\n";
print "<I>(Including 10 minute obs)<BR>(TOR is local time not UTC)</I>\n";
print "</CENTER>\n";
print "<STRONG>\n";
print "<HR>\n";

#------------------------------------------------------------------
# See if user has entered ALL as one of the stations.
#------------------------------------------------------------------

foreach (@stn) {
  if (/ALL/) {
    $get_all_stations = 1;
    $nostn = 0;
  }
}

#------------------------------------------------------------------
# Check that the station numbers are valid. If none of the station
# numbers are valid, nostn = 1 - Don't do the retrieval.
#------------------------------------------------------------------

if ($get_all_stations eq 0) {
  
  if ($lndsyn eq 1 || $ncm eq 1 || $srew eq 1 || $samosx eq 1 ||
      $esaws eq 1) {
    foreach (@stn) {
      if (/[^0-9]/ || length lt 2) {
        $_ = " ";
      } else {
        $nostn = 0;
      }
    }

  } elsif ($temp eq 1 || $pilot eq 1 || $dropsond eq 1 || $shpsyn eq 1 ||
         $bathy eq 1 || $tesac eq 1 || $tbus eq 1 || $driftr eq 1 ||
	 $buoyprof eq 1) {
    foreach (@stn) {
      if (/[^0-9A-Z]/ || length lt 1) {
        $_ = " ";
      } else {
        $nostn = 0;
      }
    }

  } elsif ($metars eq 1 || $tafs eq 1 || $atafs eq 1) {
    foreach (@stn) {
      if (/[^A-Z]/ || length lt 1) {
        $_ = " ";
      } else {
        $nostn = 0;
      }
    }

  } elsif ($tropadv eq 1) {
    foreach (@stn) {
      if (/[^A-Z]/ || length ne 4) {
        $_ = " ";
      } else {
        $nostn = 0;
      }
    }

  } elsif ($aireps eq 1 || $bogus eq 1 || $amdars eq 1) {
    foreach (@stn) {
      if (/[^0-9A-Z]/ || length lt 2) {
        $_ = " ";
      } else {
        $nostn = 0;
      }
    }
    
  } else {
    print "<H2>Subtype not recognised. subtype = " . 
    $input->{"SUBTYPE"} . "</H2>"
  }
}
    
#------------------------------------------------------------------
# If nostn = 0, at least one station number is valid, continue.
# Put the station numbers into a single variable STNS
#------------------------------------------------------------------

if ($nostn eq 0) {

  $ENV{'STNS'} = " " . @stn[0] . " " . @stn[1] . " " . @stn[2] .
                 " " . @stn[3] . " " . @stn[4] . " " . @stn[5] .  
                 " " . @stn[6] . " " . @stn[7] . " " . @stn[8] .  
                 " " . @stn[9] . " ";

#------------------------------------------------------------------
# Call the webret load executable. Output is written to $OUTFILE.
# webret exe will write the number of obs retrieved to the file
# $ENV{'TEMPFILE'}. Open, Read, Close, Delete this file & display
# the total obs. Issue warning if retrieval truncated.
#------------------------------------------------------------------

  `/home/us0400/httpd/cgi-bin/PortMetDB/web_retrieval/webret.exe > $OUTFILE 2>&1`;

  open FILE,$ENV{'TEMPFILE'};
  read FILE, $obs, 10;
  close FILE;
  unlink $ENV{'TEMPFILE'};

  print "<H3>Obs returned = " . $obs . "</H3>\n";

  if ($obs gt $input->{"MAXOBS"}) {
    print "<pre><em>\n";
    print "WARNING: This MetDB web panel retrieval allows a\n";
    print "maximum of 'maxobs' observations to be\n";
    print "retrieved. This retrieval has exceeded the limit.\n";
    print "Only the 1st 'maxobs' observations are\n";
    print "retrieved and displayed\n";
    print "</em></pre><hr><br>\n";
  }

#------------------------------------------------------------------
# Display the observations, then delete the file holding them.
#------------------------------------------------------------------

  @array=`/bin/cat $OUTFILE`;
  print @array;
  unlink $OUTFILE;

#------------------------------------------------------------------
# If nostn = 1, issue message to user.
#------------------------------------------------------------------

} else {

  if ($lndsyn eq 1 || $ncm eq 1 || $srew eq 1 || $samosx eq 1 ||
      $esaws eq 1) {
    print "<H2><CENTER>No valid station numbers entered!</H2>\n";
    print "<P>You must enter at least 1 station or block number\n";
    print "containing numerics only and left justified</P></CENTER>\n";
  
  } elsif ($temp eq 1 || $pilot eq 1 || $dropsond eq 1) {
    print "<H2><CENTER>No valid identifiers entered!</H2>\n";
    print "<P>You must enter at least 1 station number, block number\n";
    print "or callsign which must be 1 or more characters long and\n";
    print "left justified</P></CENTER>\n";

  } elsif ($shpsyn eq 1 || $bathy eq 1 || $tesac eq 1 || $driftr eq 1 || 
           $buoyprof eq 1) {
    print "<H2><CENTER>No valid ship callsigns/buoy Ids entered!</H2>\n";
    print "<P>You must enter at least 1 callsign/buoy Id which must be\n";
    print "1 or more characters long and left justified</P></CENTER>\n";
    
  } elsif ($metars eq 1 || $tafs eq 1 || $atafs eq 1) {
    print "<H2><CENTER>No valid ICAO identifiers entered!</H2>\n";
    print "<P>You must enter at least 1 ICAO identifier (full or\n";
    print "partial) which must be 1 or more characters long and\n";
    print "left justified</P></CENTER>\n";
    
  } elsif ($tropadv eq 1) {
    print "<H2><CENTER>No valid advisory identifiers entered!</H2>\n";
    print "<P>You must enter at least 1 four-figure advisory\n";
    print "identifier</P></CENTER>\n";

  } elsif ($aireps eq 1 || $amdars eq 1) {
    print "<H2><CENTER>No valid aircraft callsigns entered!</H2>\n";
    print "<P>You must enter at least 1 callsign which must be\n";
    print "2 or more characters long and left justified</P></CENTER>\n";
    
  } elsif ($bogus eq 1) {
    print "<H2><CENTER>No valid bogus identifiers entered!</H2>\n";
    print "<P>You must enter at least 1 identifier which must be\n";
    print "2 or more characters long and left justified</P></CENTER>\n";
    
  } elsif ($tbus eq 1) {
    print "<H2><CENTER>No valid tbus identifiers entered!</H2>\n";
    print "<P>You must enter at least 1 identifier which must be\n";
    print "4 or more characters long and left justified</P></CENTER>\n";
  }   
}

#------------------------------------------------------------------
# Finally, end the html
#------------------------------------------------------------------

print "</STRONG>";
print "<HR>";
print "<CENTER>";
print "<ADDRESS>";
print "email metdb\@metoffice.gov.uk";
print "</ADDRESS>";
print "</CENTER>";
print "</BODY>";
print "</HTML>";

exit 0;

#==================================================================
# Routine to get the CGI QUERY STRING
#==================================================================

sub getQuery {
  my $query = undef;
  if ($ENV{'REQUEST_METHOD'} eq 'GET') {
    $query = $ENV{'QUERY_STRING'};
  }
  elsif ($ENV{'REQUEST_METHOD'} eq 'POST') {
    read(STDIN, $query, $ENV{'CONTENT_LENGTH'});
  }
  $query;
}

#==================================================================
# Routine to parse the variables in QUERY STRING.
#==================================================================

sub parseQuery {
  my $query=shift;
  my (%input, @elements, $element, $key, $value);
  @elements=split /&/,$query;
  for $element (@elements) {
    $element =~ tr/+/ /;     # decode +'s to spaces
    $element =~ tr/a-z/A-Z/; # convert lowercase to uppercase
    ($key,$value)= split/=/, $element;
    $key  =~ s/%([\dA-Fa-f]{2})/pack("C",hex($1))/ge;    # decode key
    $value=~ s/%([\dA-Fa-f]{2})/pack("C",hex($1))/ge;    # decode value
    if (defined $input{$key}) {
      $input{$key} .= "\0$value";
    } else {
      $input{$key} = $value;
    }
  }
  \%input;
}
