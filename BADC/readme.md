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

The directory structure is as follows:

```
BADC/
  cylc/
    suites/
      BADC/
        AMDARS/
          suite.rc
        SHPSYN/
          suite.rc
        LNDSYN/
          suite.rc
        ... etc

    scripts/
      cntlchk.sh
      catchup.sh
      main.sh
      ftp.sh
      errors.sh

    utils/
      recreate_control_files.sh/py

    samples/
      amdars.ctl
      shpsyn.ctl
      ...
```

To create a `cylc` suite to execute the programs:
1. Copy the suite.rc files into the users `~/cylc-run` directory.
1. Amend the control files as necessary
1. `cylc start BADC/AMDARS` etc
