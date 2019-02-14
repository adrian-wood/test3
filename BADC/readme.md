## BADC Extraction and Transfer

### Purpose

This suite of programs performs retrievals from MetDB, creates flat files from the retrieved observations and FTPs the files to BADC via DART.

For full details see https://metnet2.metoffice.gov.uk/content/metdb-provision-data-badc-using-cylc-mdb-apps

### Fortran Code

All Fortran source is in the `source` directory, together with a `Makefile`. To create the executables:
The directory structure is as follows:

``` BADC/
  source/
    retbadc.f90
    retclm.f90
    amdout.f90
    ... 
```

To create the executables:
``` cd BADC/source
make
gfortran -c -fsyntax-only datim_mod.f90
gfortran -c -fsyntax-only zpdate_mod.f90
gfortran -c cntlchk.f90
gfortran -c datim.f90
gfortran -c zpdate_mod.f90
gfortran cntlchk.o datim.o zpdate_mod.o -o cntlchk.exe
gfortran -c retbadc.f90
gfortran -c datim_mod.f90
gfortran -c mdbret.f90
gfortran -c amdout.f90
...
gfortran retbadc.o datim_mod.o datim.o zpdate_mod.o mdbret.o amdout.o shpout.o lndout.o pltout.o ozpout.o rasout.o tmpout.o winout.o mtrout.o /var/moods/lib/MetDB_RPCv5.2_linux/MetDBRPC5.a -o retbadc.exe
gfortran -c retclm.f90
gfortran retclm.o datim_mod.o datim.o zpdate_mod.o /var/moods/lib/MetDB_RPCv5.2_linux/MetDBRPC5.a -o retclm.exe
```

Three executables will be created:
```ls -l *.exe
-rwxrwxr-x. 1 ajmoorho ajmoorho 27544 Jul 30 15:42 cntlchk.exe
-rwxrwxr-x. 1 ajmoorho ajmoorho 98304 Jul 30 15:42 retbadc.exe
-rwxrwxr-x. 1 ajmoorho ajmoorho 63680 Jul 30 15:42 retclm.exe
```

### `cylc` Scripts and Files

The BADC suite is run on `mdb-apps` by `cylc`. The `BADC/cylc` directory containes the various `suite.rc` files, shell scripts etc that are required to configure `cylc` to run the suites at the appropriate times.

The directory structure (in this repo) is as follows:

```
BADC/
  cylc/
    control_files/  <--- sample control files used by the suite, one for each datatype
      MDB.BADC.AMDARS.CONTROL
      MDB.BADC.CLIMAT.CONTROL
      ... etc

    elements/  <--- the element lists for retrieval, one for each dataype
      MDB.BADC.DATA.AMDARS
      MDB.BADC.DATA.CLIMAT
      ... etc

    include/  <--- include files for common suite definitions
      job.config

    scripts/   <--- shell scripts that the cylc suites run
      cntlchk.sh
      ftp.sh
      global_vars.sh 
      retbadc.sh
      retclm.sh

    suites/    <--- sample suite.rc files for each datatype
      BADC/
        AMDARS/
          suite.rc
        SHPSYN/
          suite.rc
        LNDSYN/
          suite.rc
        ... etc

    utils/     <--- utility scripts, not executed by the cylc suites
      check_control_files.sh
      recreate_control_files.py
      ... etc
```

To create a `cylc` suite to execute the programs:
1. Copy the `suites/BADC/*/suite.rc` files into the users `~/cylc-run/BADC` directory.
1. Copy the other directories into `/var/moods/BADC`.
1. Amend the control files as necessary.
1. Start the suite by issuing `cylc start BADC/AMDARS` etc.
