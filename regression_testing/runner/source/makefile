#-----------------------------------------------------------------------
#
# FILE NAME      : makefile
#
# TYPE           : makefile
#
# REVISION INFO  :
#
#

FF = ifort
FFLAGS =

MDBRTVL = mdbrtvl.exe
RETBUFR = retbufr.exe
GETGRIB = getgrib.exe
RPCVER = 5.2
#RPCLIB =  $(shell find $(BASE) -name *.a -print)
RPCLIB = ../bin/MetDBRPCv$(RPCVER)/MetDBRPC5.a

MDBRTVL_O = mdbrtvl.o \
            getname.o

RETBUFR_O= retbufr.o \
           countbmsg.o \
           get1bmsg.o \
           ichar3.o 

GETGRIB_O = getgrib.o


ALL : $(MDBRTVL) $(RETBUFR) $(GETGRIB) $(CLEAN)
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


%.o : %.f90 
	$(FF) -c $<

%.o : %.F90 
	$(FF) -c $(FFLAGS) $<
