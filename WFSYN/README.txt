                    ===============================
                       UK Meteorological Office
                      WFSYN Windfarm Plus Test Program
                           metdb@metoffice.gov.uk
                    ===============================

        (C) CROWN COPYRIGHT 2016 - MET OFFICE. All Rights Reserved.
                      Met Office, United Kingdom
       The use, duplication and disclosure of this code is strictly
     prohibited without the permission of The Meteorological Database 
                      Team at the above address.
                    ===============================

----------------------------------------------------------------------------
Contents:
----------------------------------------------------------------------------
1. Background
2. Package Contents
3. Compilation
4. Execution
5. End

----------------------------------------------------------------------------
1. Background
----------------------------------------------------------------------------
Windfarm datatypes are often short-lived which means the end-to-end process for
producing the WPF+ product may get out of date. To reduce this risk the plan
is to create a "dummy" windfarm data product which will run continuously in an 
end-to-end test. 

This package contains a program WFSYN.f90 which:
a) Retrieves the single "latest" LNDYSN data for Heathrow
b) Encodes a BUFR file from that information using the Windfarm sequence.

The program will run at hh:15 and the subsequent BUFR file will be tranferred
via DART to the MetDB systems where it will be ingested as the WFSYN datatype.

----------------------------------------------------------------------------
2. Package Contents
----------------------------------------------------------------------------
WFSYN.f90        - source code of the RPC retrieval / BUFR encoding program.
run_wfsyn.sh     - shell script to run the above, following compilation.
WFSYN_README.txt - this file.

----------------------------------------------------------------------------
3. Compilation
----------------------------------------------------------------------------
The program requires both the MetDB RPC Client package and the MetDB BUFR 
package. Both of these packages must have been retrieved and installed on
the machine you intend to compile (and run) this progam on.

Please see MetDB Documentation for further information about these packages:

https://metnet2.metoffice.gov.uk/content/metdb-technote-10-mdb-users-guide-remote-procedure-calls-rpc
https://metnet2.metoffice.gov.uk/content/metdb-technote-1a-bufr-package

Now you can compile the program to create the executable wfsyn.exe using
a command similar to the following: 

  ifort wfsyn.f90 MetDB_BUFR24.0.00/libbufr.a MetDB_RPCv5.2_linux/MetDBRPC5.a -o wfsyn.exe

----------------------------------------------------------------------------
4. Execution
----------------------------------------------------------------------------

The usual MetDB RPC Client environment variables should be set, plus the 
"BUFR_LIBRARY" variable which should point to the location of the BUFR 
tables (rRefer to package documentation for further information).

The program writes messages to STDOUT and produces a file WFSYN.bufr which
can then be FTPd or whatever to the required location.

It is intended that the shell script run_wfsyn.sh is used to execute the 
program using cron at hh:15. *Modify the shell script as appropriate*, and
run it with a cron entry similar to the following:

15 * * * * ~usmdb/WFSYN/run_wfsyn.sh >>/tmp/wfsyn_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1

----------------------------------------------------------------------------
5. End
----------------------------------------------------------------------------
