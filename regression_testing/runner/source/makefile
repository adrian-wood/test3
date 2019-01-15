#-----------------------------------------------------------------------
#
# FILE NAME      : makefile
#
# TYPE           : makefile
#
# REVISION INFO  :
#
#

FF = gfortran
FFLAGS =

MDBRTVL = mdbrtvl.exe
NCDFRTVL = ncdfrtvl.exe
RETBUFR = retbufr.exe
GETGRIB = getgrib.exe
RPCVER = 5.2
RPCLIB = /var/moods/lib/MetDB_RPCv5.2_linux/MetDBRPC5.a

MDBRTVL_O = mdbrtvl.o \
            getname.o

RETBUFR_O= retbufr.o \
           countbmsg.o \
           get1bmsg.o \
           ichar3.o 

NCDFRTVL_O = ncdfrtvl.o

GETGRIB_O = getgrib.o


ALL : $(MDBRTVL) $(RETBUFR) $(GETGRIB) $(NCDFRTVL) $(CLEAN)
CLEAN :
	rm *.o

$(MDBRTVL) : $(MDBRTVL_O)
	$(FF) -o $@ $^ $(RPCLIB)
	mv $@ ../bin/.

$(RETBUFR) : $(RETBUFR_O)
	$(FF) -o $@ $^ $(RPCLIB)
	mv $@ ../bin/.

$(GETGRIB) : $(GETGRIB_O)
	$(FF) -o $@ $^ $(RPCLIB)
	mv $@ ../bin/.

$(NCDFRTVL) : $(NCDFRTVL_O)
	$(FF) -o $@ $^ $(RPCLIB)
	mv $@ ../bin/.

%.o : %.f90 
	$(FF) -c $<

%.o : %.F90 
	$(FF) -c $(FFLAGS) $<
