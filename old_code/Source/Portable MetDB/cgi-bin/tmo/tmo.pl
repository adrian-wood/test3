#!/usr/bin/perl
#------------------------------------------------------------------
#
# ROUTINE       : tmo.pl
#
# LANGUAGE      : PERL
#
# DESCRIPTION   : CGI script for MetDB Ten Minute Observations (TMO)
#               : retrieval from HPMDB on us0400.
#
# CALLS         : tmo.exe
#
# REVISION INFO :
#
# $Revision: 3$
# $Date: 28/03/2007 11:01:20$
# $Source: /home/us0400/httpd/cgi-bin/PortMetDB/tmo/RCS/tmo.pl,v $
#
# CHANGE RECORD :
#
# $Log:
#  3    Met_DB_Project 1.2         28/03/2007 11:01:20    Rosemary Lavery
#       Updated variable to give valid link to gif (ref to us0400 now out of
#       date: HP server decommissioned).
#  2    Met_DB_Project 1.1         15/09/2006 15:26:38    Stan Kellett
#       Corrected link to PortMetDB folder as was still linked to the
#       TestPortMetDB folder
#  1    Met_DB_Project 1.0         14/09/2006 15:44:54    Stan Kellett    
# $
# Revision 1.18  2003/02/03 12:53:21  usmdb
# Added environment variables METDB_BASE_DIR and
# METDB_ELEMIDX - S.Cox
#
# Revision 1.17  2001/05/23 11:42:48  usmdb
# Added PW.
#
# Revision 1.16  2001/04/10 13:39:15  usmdb
# *** empty log message ***
#
# Revision 1.15  2001/04/02 11:00:50  usmdb
# Added METDB_TMO_HOURSBACK ENV VAR - S.Cox
#
# Revision 1.14  2001/03/30 09:41:31  usmdb
# *** empty log message ***
#
# Revision 1.13  2001/03/30 09:37:17  usmdb
# Mucked about with order again.
#
# Revision 1.12  2001/03/28 11:07:29  usmdb
# *** empty log message ***
#
# Revision 1.11  2001/03/28 08:34:39  usmdb
# *** empty log message ***
#
# Revision 1.10  2001/03/28 08:10:02  usmdb
# *** empty log message ***
#
# Written by Simon Cox 16-01-2001
#------------------------------------------------------------------

print "Content-type: text/html\n\n";

my $query = getQuery();
my $input = parseQuery($query);

#------------------------------------------------------------------
# Set up environment variables for use in tmo.exe routine.
# Update vrbl .._TMO_PUBDIR from us0400 to www-metdb  (28/03/07 !3)  
#------------------------------------------------------------------

umask 0000;

$ENV{'METDB_BASE_DIR'} = "/home/us0400/mdb_new/op";
$ENV{'METDB_ELEMIDX'} = "/home/us0400/mdb_new/op/data/element.index";

$ENV{'METDB_TMO_STATION'} = $input->{"METDB_TMO_STATION"};
#$ENV{'METDB_TMO_STATION'} = '03772';

$ENV{'METDB_TMO_HOURSBACK'} = $input->{"METDB_TMO_HOURSBACK"};
#$ENV{'METDB_TMO_HOURSBACK'} = '12';

$ENV{'METDB_TMO_DATDIR'} = "/home/us0400/usmdb/public_html/tmo/data/" . $$;
mkdir($ENV{'METDB_TMO_DATDIR'},0777);
$METDB_TMO_PUBDIR = "http://www-metdb/~usmdb/tmo/data/" . $$;

$ENV{'METDB_TMO_DATFILE'} = $ENV{'METDB_TMO_DATDIR'} . "/data";
$OUTFILE = "/var/tmp/" . $$ . "tmo.txt";
$ENV{'INPUT_FILE'} = $ENV{'METDB_TMO_DATFILE'};
$ENV{'OUTPUT_DIR'} = $ENV{'METDB_TMO_DATDIR'} . "/";

#------------------------------------------------------------------
# HTML header.
#------------------------------------------------------------------

html_header();

#------------------------------------------------------------------
# Check validity of station.
#------------------------------------------------------------------

$_ = $ENV{'METDB_TMO_STATION'};
if (/[^0-9]/ || length lt 5) {
  print "ERROR: Invalid station ID entered. ID should be 5 \n";
  print "characters long and only contain characters 0-9\n";
  html_footer();
  exit 1;
}

#------------------------------------------------------------------
# Check validity of hoursback.
#------------------------------------------------------------------

if ($ENV{'METDB_TMO_HOURSBACK'} ne 12 && $ENV{'METDB_TMO_HOURSBACK'} ne 24 && 
    $ENV{'METDB_TMO_HOURSBACK'} ne 36 && $ENV{'METDB_TMO_HOURSBACK'} ne 48 ) {
  print "ERROR: Invalid Time series period selected. Valid values are 12, 24, 26 or 48\n";
  html_footer();
  exit 1;
}

#------------------------------------------------------------------
# Call the tmo executable. Check that the output data file was
# created. If not, error in tmo.exe
#------------------------------------------------------------------

`/bin/nice -n 15 /home/us0400/httpd/cgi-bin/PortMetDB/tmo/tmo.exe > $OUTFILE 2>&1`;

@array=`/bin/cat $OUTFILE`;
print "<pre>\n";
print @array;
print "</pre>\n";
unlink $OUTFILE;

if ( ! -e $ENV{'METDB_TMO_DATFILE'} ) {
  html_footer();
  exit 1;
}

#------------------------------------------------------------------
# Call TMO_plot to create images.
#------------------------------------------------------------------

`/bin/nice -n 15 /usr/local/bin/wave /home/us0400/httpd/cgi-bin/PortMetDB/tmo/TMO_plot >> $OUTFILE 2>&1`;

#print "\n";
#@array=`/bin/cat $ENV{'METDB_TMO_DATFILE'}`;
#print @array;

#------------------------------------------------------------------
# Display images.
#------------------------------------------------------------------

display_images();

#------------------------------------------------------------------
# HTML footer.
#------------------------------------------------------------------

html_footer();

#------------------------------------------------------------------
# Remove old directories. Those older than 10 mins (600 seconds)
#------------------------------------------------------------------

chdir ($ENV{'METDB_TMO_DATDIR'});
chdir ('..');

@dir=`ls`;
foreach $x (@dir){
  chomp $x;
  ($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks)=stat($x);
  $since=time-$atime;
  if ($since > 600) {
    `rm -rf $x`;
  }
}

append_to_log();

#------------------------------------------------------------------
# End of routine. Exit.
#------------------------------------------------------------------

exit 0;

#==================================================================
# Routine to write html header section.
#==================================================================

sub html_header {
my $minutes,$hour,$dummy,$day,$month,$year;

($dummy,$minutes,$hour,$day,$month,$year)=localtime(time);
$year=$year+1900;
$month=$month+1;
if ($minutes < 10) {$minutes="0".$minutes;};
   
print <<ENDP;
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<meta name="Author" content="John Hodkinson">
<meta http-equiv="refresh" content="600">
<meta http-equiv="pragma" content="no-cache">
<title>TMO display</title>
<!-- the above commands refresh the page after 600 seconds, and disable the caching of the page -->
<script LANGUAGE="JavaScript">
        <!-- hiding
        var i = 601
        var j = 1
        function startseccount(){
        i = i - j
        document.form1.clock.value = i
        timerID = setTimeout("startseccount()", 1000) 
        }
        //      end hiding -->
</script>
<!-- in the above script, timeout counts up to one second in milliseconds -->
$REMOTE_HOST
</head>

<body ONLOAD="startseccount()">
<b><font size="-2" color="#000000" face="Helvetica">

<h1 align="center"></font><font color="#000000" face="Helvetica" size="5">MetDB Team - SYNOP display</h1>
<h2 align="center">Station $ENV{'METDB_TMO_STATION'} at $hour:$minutes $day\/$month\/$year</h2>
ENDP
}

#==================================================================
# Routine to write html footer section.
#==================================================================

sub html_footer {
print <<ENDP;
<hr>
<address>
email metdb\@metoffice.gov.uk
</address>
</body>
</html>
ENDP
}

#==================================================================
# Routine to display images.
#==================================================================

sub display_images {
my ($pw,$OUTFILE,$pres);
$OUTFILE=$ENV{'METDB_TMO_DATFILE'};
$pw=`tail -n1 $OUTFILE`;
@pw=unpack("A10,A6,A13",$pw);
$pw[2]=int($pw[2]);
$pres="<center><h2>Present Weather";
if ($pw[2] > 99 && $pw[2] < 200) {$pres=$pres." - (Automatic)";} elsif ($pw[2] < 100) {$pres=$pres." - (Manual)";}
$pres=$pres."</h2></center><b><ul>At $pw[1] MetDB Code $pw[2]:</b><br><ul><li>".present($pw[2])."</li></ul></ul>";
print <<ENDP;
<div align="center"><center>

<table BORDER="1" COLS="2" WIDTH="100%">
  <tr>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/4.gif"></td>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/3.gif"></td>
  </tr>
  <tr>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/1.gif"></td>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/2.gif"></td>
  </tr>
  <tr>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/6.gif"></td>
    <td>$pres</td>
  </tr>
  <tr>
    <td align="center" colspan=2><img SRC="$METDB_TMO_PUBDIR/7.gif"></td>
  </tr>
  <tr>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/9.gif"></td>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/8.gif"></td>
  </tr>
  <tr>
    <td align="center"><img SRC="$METDB_TMO_PUBDIR/5.gif"></td>
    <td align="center">&nbsp</td>
  </tr>
</table>
</center></div></b>

<form name="form1">
  <b><div align="center"><center><p>This page will reload in approx.&nbsp;<input type="text"
  name="clock" size="3" value
  style="font-family: Helvetica; font-size: 10; font-weight: bold"> seconds.&nbsp;</b></p>
  </center></div></b>
</form>
ENDP
}

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

#==================================================================
# Routine to append to log file
#==================================================================

sub append_to_log(){
  my ($message,@date,$x);
  open (LOGFILE,">>/home/us0400/httpd/cgi-bin/PortMetDB/tmo/user_log");
  @date=localtime(time);
  $date[5]=$date[5]+1900;
  $date[4]=$date[4]+1;
  for ($x=1;$x<5;$x++) {
    if ($date[$x] < 10) {
      $date[$x]="0".$date[$x];
    }
  }
  
  $message="$date[3]/$date[4]/$date[5] $date[2]:$date[1] - ";
  $message=$message."$ENV{'REMOTE_ADDR'} - $ENV{'METDB_TMO_STATION'}";
  print LOGFILE $message,"\n";
  close(LOGFILE);
}

#==================================================================
# Routine to convert present weather code to text
#==================================================================

sub present($code) {
my $code=shift(@_);

%present_weather=(
0 => "Cloud development not observed or not observable. Characteristic change of the state of sky during the past hour",
1 => "Clouds generally dissolving or becoming less developed. Characteristic change of the state of sky during the past hour",
2 => "State of sky on the whole unchanged. Characteristic change of the state of sky during the past hour",
3 => "Clouds generally forming or developing. Characteristic change of the state of sky during the past hour",
4 => "Visibility reduced by smoke, e.g. veldt or forest fires, Industrial smoke or volcanic ashes",
5 => "Haze",
6 => "Widespread dust in suspension in the air, not raised by wind at or near the station at the time of observation",
7 => "Dust or sand raised by wind at or near the station at the time of observation, but no well-developed dust whirl(s) or sand whirl(s), and no duststorm or sandstorm seen; or, In the case of sea stations and coastal stations, blowing spray at the station ",
8 => "Well-developed dust whirl(s) or sand whirl(s) seen at or near the station during the preceding hour or at the same time of observation, but no duststorm or sandstorm",
9 => "Duststorm or sandstorm within sight at the time of observation, or at the station during the preceding hour",
10 => "Mist. Shallow fog or Ice fog at the station, whether on land or sea, not deeper than about 2 metres on land or 10 metres at sea",
11 => "Patches. Shallow fog or Ice fog at the station, whether on land or sea, not deeper than about 2 metres on land or 10 metres at sea",
12 => "More or less continuous",
13 => "Lightning visible, no thunder heard",
14 => "Precipitation within sight, not reaching the ground or the surface of the sea",
15 => "Precipitation within sight, reaching the ground or the surface of the sea, but distant, i.e. estimated to be more than 5 km from the station",
16 => "Precipitation within sight, reaching the ground or the surface of the sea, near to, but not at the station ",
17 => "Thunderstorm, but no precipitation at the time of observation ",
18 => "Squalls. At or within sight of the station during the preceding hour or at the time of observation",
19 => "Funnel cloud(s). At or within sight of the station during the preceding hour or at the time of observation",
20 => "Drizzle (not freezing) or snow grains not falling as shower(s)",
21 => "Rain (not freezing) not falling as shower(s)<BR><li>In preceding hour but not at time of observation.</li>",
22 => "Snow not falling as shower(s)<BR><li>In preceding hour but not at time of observation.</li>",
23 => "Rain and snow or ice pellets not falling as shower(s)<BR><li>In preceding hour but not at time of observation.</li>",
24 => "Freezing drizzle or freezing rain not falling as shower(s)<BR><li>In preceding hour but not at time of observation.</li>",
25 => "Shower(s) of rain<BR><li>In preceding hour but not at time of observation.</li>",
26 => "Shower(s) of snow, or of rain and snow<BR><li>In preceding hour but not at time of observation.</li>",
27 => "Shower(s) of hail, or of rain and hail<BR><li>In preceding hour but not at time of observation.</li>",
28 => "Fog or Ice fog<BR><li>In preceding hour but not at time of observation.</li>",
29 => "Thunderstorm (with or without precipitation)<BR><li>In preceding hour but not at time of observation.</li>",
30 => "Slight or moderate duststorm or sandstorm - has decreased during the preceding hour",
31 => "Slight or moderate duststorm or sandstorm - no appreciable change during the preceding hour",
32 => "Slight or moderate duststorm or sandstorm - has begun or has increased during the preceding hour",
33  => "Severe duststorm or sandstorm - has decreased during the preceding hour",
34  => "Severe duststorm or sandstorm - no appreciable change during the preceding hour",
35  => "Severe duststorm or sandstorm - has begun or has increased during the preceding hour",
36 => "Slight or moderate drifting snow - generally low (below eye level)",
37 => "Heavy drifting snow - generally low (below eye level)",
38 => "Slight or moderate blowing snow - generally high(above eye level)",
39 => "Heavy blowing snow - generally high(above eye level)",
40 => "Fog or ice fog at a distance at the time of observation, but not at the station during the preceding hour, the fog or ice fog extending to a level above that of the observer",
41 => "Fog or ice fog in patches",
42 => "Fog or ice fog, sky visible - has become thinner during the preceding hour",
43 => "Fog or ice fog, sky invisible - has become thinner during the preceding hour",
44 => "Fog or ice fog, sky visible - no appreciable change during the preceding hour",
45 => "Fog or ice fog, sky invisible  - no appreciable change during the preceding hour",
46 => "Fog or ice fog, sky visible - has begun or has become thicker during the preceding hour",
47 => "Fog or ice fog, sky invisible - has begun or has become thicker during the preceding hour",
48 => "Fog, depositing rime, sky visible",
49 => "Fog, depositing rime, sky invisible",
50 => "Drizzle, not freezing, intermittent - slight at time of observation",
51 => "Drizzle, not freezing, continuous - slight at time of observation",
52 => "Drizzle, not freezing, intermittent - moderate at time of observation",
53 => "Drizzle, not freezing, continuous - moderate at time of observation",
54 => "Drizzle, not freezing, intermittent - heavy (dense) at time of observation",
55 => "Drizzle, not freezing, continuous - heavy (dense) at time of observation",
56 => "Drizzle, freezing, slight",
57 => "Drizzle, freezing, moderate or heavy (dense)",
58 => "Drizzle and rain, slight",
59 => "Drizzle and rain, moderate or heavy",
60 => "Rain, not freezing, intermittent - slight at time of observation",
61 => "Rain, not freezing, continuous - slight at time of observation",
62 => "Rain, not freezing, intermittent - moderate at time observation",
63 => "Rain, not freezing, continuous - moderate at time observation",
64 => "Rain, not freezing, intermittent - heavy at time of observation",
65 => "Rain, not freezing, continuous - heavy at time of observation",
66 => "Rain, freezing, slight",
67 => "Rain, freezing, moderate or heavy",
68 => "Rain or drizzle and snow, light",
69 => "Rain or drizzle and snow, moderate or heavy",
70 => "Intermittent fall of snowflakes - slight at time of observation",
71 => "Continuous fall of snowflakes - slight at time of observation",
72 => "Intermittent fall of snowflakes - moderate at time observation",
73 => "Continuous fall of snowflakes - moderate at time observation",
74 => "Intermittent fall of snowflakes - heavy at time of observation ",
75 => "Continuous fall of snowflakes - heavy at time of observation",
76 => "Diamond dust (with or without fog)",
77 => "Snow grains (with or without fog)",
78 => "Isolated star-like snow crystals (with or without fog)",
79 => "Ice pellets",
80 => "Rain shower(s), slight",
81 => "Rain shower(s), moderate or heavy",
82 => "Rain shower(s), violent",
83 => "Shower(s) of rain and snow mixed, slight",
84 => "Shower(s) of rain and snow mixed, moderate or heavy",
85 => "Snow shower(s), slight",
86 => "Snow shower(s), moderate or heavy",
87 => "Shower(s) of snow pellets or small hail, with or without rain or rain and snow mixed - slight",
88 => "Shower(s) of snow pellets or small hail, with or without rain or rain and snow mixed - moderate or heavy",
89 => "Shower(s) of hail, with or without rain or rain and snow mixed, not associated with thunder - slight",
90 => "Shower(s) of hail, with or without rain or rain and snow mixed, not associated with thunder - moderate or heavy",
91 => "Slight rain at time of observation - Thunderstorm during the preceding hour but not at time of observation",
92 => "Moderate or heavy rain at time of observation - Thunderstorm during the preceding hour but not at time of observation",
93 => "Slight snow, or rain and snow mixed or hail at time of observation  - Thunderstorm during the preceding hour but not at time of observation",
94 => "Moderate or heavy snow, or rain and snow mixed or hail at time of observation  - Thunderstorm during the preceding hour but not at time of observation",
95 => "Thunderstorm, slight or moderate, without hail*, but with rain and/or snow at time of observation - Thunderstorm at time of observation. ",
96 => "Thunderstorm, slight or moderate, with hail* at time of observation  - Thunderstorm at time of observation.",
97 => "Thunderstorm, heavy, without hail*, but with rain and/or snow at time of observation  - Thunderstorm at time of observation.",
98 => "Thunderstorm combined with duststorm or sandstorm at time of observation - Thunderstorm at time of observation.",
99 => "Thunderstorm, heavy, with hail* at time of observation - Thunderstorm at time of observation.",
100 => "No significant weather observed",
101 => "Clouds generally dissolving or becoming less developed during the past hour",
102 => "State of sky on the whole unchanged during the past hour",
103 => "Clouds generally forming or developing during the past hour",
104 => "Haze or smoke, or dust in suspension in the air, visibility equal to, or greater than, 1km",
105 => "Haze or smoke, or dust In suspension in the air, visibility less than 1 km",
106 => "Reserved",
107 => "Reserved",
108 => "Reserved",
109 => "Reserved",
110 => "Mist",
111 => "Diamond dust",
112 => "Distant lightning",
113 => "Reserved",
114 => "Reserved",
115 => "Reserved",
116 => "Reserved",
117 => "Reserved",
118 => "Squalls",
119 => "Reserved",
120 => "Fog<BR><li>In preceding hour but not at time of observation.</li>",
121 => "PRECIPITATION<BR><li>In preceding hour but not at time of observation.</li>",
122 => "Drizzle (not freezing) or snow grains<BR><li>In preceding hour but not at time of observation.</li>",
123 => "Rain (not freezing)<BR><li>In preceding hour but not at time of observation.</li>",
124 => "Snow<BR><li>In preceding hour but not at time of observation.</li>",
125 => "Freezing drizzle or freezing rain<BR><li>In preceding hour but not at time of observation.</li>",
126 => "Thunderstorm (with or without precipitation)<BR><li>In preceding hour but not at time of observation.</li>",
127 => "Blowing OR DRIFTING SNOW OR SAND",
128 => "Blowing or drifting snow or sand, visibility equal to, or greater than, 1 km",
129 => "Blowing or drifting snow or sand, visibility less than 1 km",
130 => "FOG",
131 => "Fog or ice fog In patches",
132 => "Fog or ice fog, has become thinner during the past hour",
133 => "Fog or ice fog, no appreciable change during the past hour",
134 => "Fog or ice fog, has begun or become thicker during the past hour",
135 => "Fog, depositing rime",
136 => "Reserved",
137 => "Reserved",
138 => "Reserved",
139 => "Reserved",
140 => "PRECIPITATION",
141 => "Precipitation, slight or moderate",
142 => "Precipitation, heavy",
143 => "Liquid precipitation, slight or moderate",
144 => "Liquid precipitation, heavy",
145 => "Solid precipitation, slight or moderate",
146 => "Solid precipitation, heavy",
147 => "Freezing precipitation, slight or moderate",
148 => "Freezing precipitation, heavy",
149 => "Reserved",
150 => "DRIZZLE",
151 => "Drizzle, not freezing, slight",
152 => "Drizzle, not freezing, moderate",
153 => "Drizzle, not freezing, heavy",
154 => "Drizzle, freezing, slight",
155 => "Drizzle, freezing, moderate",
156 => "Drizzle, freezing, heavy",
157 => "Drizzle and rain, slight",
158 => "Drizzle and rain, moderate or heavy",
159 => "Reserved",
160 => "RAIN",
161 => "Rain, not freezing, slight",
162 => "Rain, not freezing, moderate",
163 => "Rain, not freezing, heavy",
164 => "Rain, freezing, slight",
165 => "Rain, freezing, moderate",
166 => "Rain, freezing, heavy",
167 => "Rain (or drizzle) and snow, slight",
168 => "Rain (or drizzle) and snow, moderate or heavy",
169 => "Reserved",
170 => "SNOW",
171 => "Snow, slight",
172 => "Snow, moderate",
173 => "Snow, heavy",
174 => "Ice pellets, slight",
175 => "Ice pellets, moderate",
176 => "Ice pellets, heavy",
177 => "Reserved",
178 => "Reserved",
179 => "Reserved",
180 => "SHOWER(S) or intermittent PRECIPITATION",
181 => "Rain shower(s) or intermittent rain, slight",
182 => "Rain shower(s) or intermittent rain, moderate",
183 => "Rain shower(s) or intermittent rain, heavy",
184 => "Rain shower(s) or intermittent rain, violent",
185 => "Snow shower(s) or intermittent snow, slight",
186 => "Snow shower(s) or intermittent snow, moderate",
187 => "Snow shower(s) or intermittent snow, heavy",
188 => "Reserved",
189 => "Reserved",
190 => "THUNDERSTORM",
191 => "Thunderstorm, slight or moderate, with no precipitation",
192 => "Thunderstorm, slight or moderate, with rain showers and/or snow showers",
193 => "Thunderstorm, slight or moderate, with hail ",
194 => "Thunderstorm, heavy, with no precipitation",
195 => "Thunderstorm, heavy, With rain showers and/or snow showers",
196 => "Thunderstorm, heavy, with hail",
197 => "Reserved",
198 => "Reserved",
199 => "Tornado",
508 => "No significant phenomenon to report, present and past weather omitted",
509 => "No observation, data not available, present and past weather omitted",
510 => "Present and past weather missing, but expected",
511 => "Missing value"
);
$present=$present_weather{$code};
return $present;
}
