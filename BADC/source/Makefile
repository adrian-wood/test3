#--------------------------------------------------------------------------
#
# FILE NAME : Makefile
#
# TYPE : Makefile
#
# PURPOSE : To build the routines and programs for the provision of data
#           to British Atmospheric Data Centre (BADC).
#
# USAGE :
# make - creates 3 executables cntlchk.exe, retbadc.exe and retclm.exe,
#        copy to desired location.
# make all/ALL - as above.
# make cntlchk.exe/retbadc.exe/retclm.exe - creates individual executables.
# make clean - Tidy all .o, .mod and .exe files leaving source directory 
#              ready for push to repo.
#
# NOTES :
# 1. The code is reliant on the MetDB RPC library, specified by LIB
#
# REVISION INFO :
# MB-575  Initial conversion of code on z/OS to Linux     Andy Moorhouse
#
#--------------------------------------------------------------------------

FC=gfortran
LIB=/var/moods/lib/MetDB_RPCv5.2_linux/MetDBRPC5.a

ALL = cntlchk.exe retbadc.exe retclm.exe
CHKOBJS = cntlchk.o datim.o zpdate_mod.o
RETOBJS = retbadc.o datim_mod.o datim.o zpdate_mod.o mdbret.o amdout.o shpout.o \
          lndout.o pltout.o ozpout.o rasout.o tmpout.o winout.o mtrout.o
CLMOBJS = retclm.o datim_mod.o datim.o zpdate_mod.o

.SUFFIXES:
.SUFFIXES: .o .f90

ALL all: ${ALL}

cntlchk.exe: ${CHKOBJS}
	$(FC) $^ -o $@

retbadc.exe: ${RETOBJS}
	$(FC) $^ ${LIB} -o $@

retclm.exe: ${CLMOBJS}
	$(FC) $^ ${LIB} -o $@

retbadc.f90 cntlchk.f90 retclm.f90: datim_mod.mod zpdate_mod.mod

%.o: %.f90
	$(FC) -c $<

%.mod: %.f90
	$(FC) -c -fsyntax-only $<

clean:
	rm -rf ${CHKOBJS} ${RETOBJS} ${CLMOBJS} *.exe *.mod
