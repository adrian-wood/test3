FC=gfortran
LIB=/var/moods/lib/MetDB_RPCv5.2_linux/MetDBRPC5.a

ALL = cntlchk.exe retbadc.exe retclm.exe
CHKOBJS = cntlchk.o datim.o zpdate_mod.o
RETOBJS = retbadc.o datim_mod.o datim.o zpdate_mod.o mdbret.o amdout.o shpout.o \
          lndout.o pltout.o ozpout.o ozsout.o rasout.o tmpout.o winout.o mtrout.o
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
