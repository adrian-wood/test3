# 
# Makefile to create the files needed for Station search
#
#$Header: Makefile, 2, 03/02/2014 16:22:01, John Norton$
#
# Written by Stanley Kellett, MetDB team 25/9/2000
# Updated to include create_nimbus_station list 19/2/2001
# Updated for recision Control 19/2/2001
#
#$Source: /home/h01/usmdb/station_search/source/RCS/Makefile,v $
#$Date: 03/02/2014 16:22:01$
#$Revision: 2$
#
#$Log:
# 2    Met_DB_Project 1.1         03/02/2014 16:22:01    John Norton
#      Changed references to us0400 to use h01.
# 1    Met_DB_Project 1.0         13/11/2006 16:19:01    Kudsia Gwangwaa 
#$
#Revision 1.3  2002/12/19 13:10:03  usmdb
#fixed Makefile so that it now runs ok.
#Stan Kellett - 19th December 2002.
#
#Revision 1.2  2001/09/04 12:53:00  usmdb
#Makefile now compiles and links using f90 compiler
#
#Revision 1.1  2001/02/19 14:55:01  usmdb
#Initial revision
#
#

# Define Directory which holds the sources code
sources=/home/h01/usmdb/station_search/source

# Define the directory which will hold the object modules
objects=/home/h01/usmdb/station_search/objects

# Define the directory to hold the executables
exes=/home/h01/usmdb/station_search/exe

# Define the directory to hold the c code header files
headers=/home/h01/usmdb/station_search/headers

all: $(exes)/icao_search.exe $(exes)/wmo_search.exe $(exes)/name_search.exe $(exes)/display_full_wmo_details.exe $(exes)/display_full_icao_details.exe

# If any of the objects for icao_search.exe have changed then re-link
$(exes)/icao_search.exe: $(objects)/icao_search.o $(objects)/write_html_head.o $(objects)/write_icao_details.o $(objects)/write_html_tail.o 
	ifort -g -O -nus -nofor_main -w -o $(exes)/icao_search.exe      \
          $(objects)/icao_search.o        \
          $(objects)/write_html_head.o    \
          $(objects)/write_icao_details.o \
          $(objects)/write_html_tail.o /home/h01/mdb_new/op/lib/source/mdb.a

# If any of the objects for icao_search.exe have changed then re-link
$(exes)/wmo_search.exe: $(objects)/wmo_search.o $(objects)/write_html_head.o $(objects)/write_abbv_details.o $(objects)/write_html_tail.o 
	ifort -g -O -nus -nofor_main -w -o $(exes)/wmo_search.exe      \
          $(objects)/wmo_search.o        \
          $(objects)/write_html_head.o    \
          $(objects)/write_abbv_details.o \
          $(objects)/write_html_tail.o /home/h01/mdb_new/op/lib/source/mdb.a

# If any of the objects for icao_search.exe have changed then re-link
$(exes)/name_search.exe: $(objects)/name_search.o $(objects)/check_pattern.o $(objects)/write_html_head.o $(objects)/write_name_details.o $(objects)/write_html_tail.o 
	ifort -g -O -nus -nofor_main -w -o $(exes)/name_search.exe      \
          $(objects)/name_search.o        \
          $(objects)/write_html_head.o    \
	  $(objects)/check_pattern.o \
          $(objects)/write_name_details.o \
          $(objects)/write_html_tail.o /home/h01/mdb_new/op/lib/source/mdb.a
	  
# If any of the objects for display_full_wmo_details.exe have changed then re-link
$(exes)/display_full_wmo_details.exe: $(objects)/write_html_head.o $(objects)/write_html_tail.o $(objects)/write_full_wmo_details.o $(objects)/part_string.o $(objects)/display_full_wmo_details.o
	ifort -g -O -nus -nofor_main -w -o $(exes)/display_full_wmo_details.exe \
         $(objects)/write_html_head.o \
         $(objects)/write_html_tail.o \
         $(objects)/write_full_wmo_details.o \
	 $(objects)/part_string.o \
         $(objects)/display_full_wmo_details.o /home/h01/mdb_new/op/lib/source/mdb.a

# Create display_full_icao_details.exe, if any of the object modules
# used to make this executable have changed then relink
$(exes)/display_full_icao_details.exe: $(objects)/write_html_head.o $(objects)/write_html_tail.o $(objects)/write_full_icao_details.o $(objects)/part_string.o $(objects)/display_full_icao_details.o
	ifort -g -O -nus -nofor_main -w -o $(exes)/display_full_icao_details.exe \
	$(objects)/display_full_icao_details.o \
	$(objects)/write_html_head.o \
	$(objects)/write_html_tail.o \
	$(objects)/write_full_icao_details.o \
	$(objects)/part_string.o \
	$(objects)/display_full_icao_details.o /home/h01/mdb_new/op/lib/source/mdb.a
	 
$(objects)/display_full_icao_details.o: $(sources)/display_full_icao_details.c
	cc -c -o $(objects)/display_full_icao_details.o $(sources)/display_full_icao_details.c
	 
$(objects)/write_full_icao_details.o: $(sources)/write_full_icao_details.c
	cc -c -o $(objects)/write_full_icao_details.o $(sources)/write_full_icao_details.c
	 
$(objects)/display_full_wmo_details.o: $(sources)/display_full_wmo_details.c
	cc -c -o $(objects)/display_full_wmo_details.o $(sources)/display_full_wmo_details.c

$(objects)/write_full_wmo_details.o: $(sources)/write_full_wmo_details.c
	cc -c -o $(objects)/write_full_wmo_details.o $(sources)/write_full_wmo_details.c

$(objects)/part_string.o: $(sources)/part_string.c 
	cc -c -o $(objects)/part_string.o $(sources)/part_string.c

$(objects)/icao_search.o: $(sources)/icao_search.F
	ifort -g -nus -w -c -o $(objects)/icao_search.o $(sources)/icao_search.F

$(objects)/name_search.o: $(sources)/name_search.F
	ifort -g -nus -w -c -o $(objects)/name_search.o $(sources)/name_search.F

$(objects)/write_html_head.o: $(sources)/write_html_head.F
	ifort -g -nus -w -c -o $(objects)/write_html_head.o $(sources)/write_html_head.F

$(objects)/write_icao_details.o: $(sources)/write_icao_details.F
	ifort -g -nus -w -c -o $(objects)/write_icao_details.o $(sources)/write_icao_details.F

$(objects)/write_html_tail.o: $(sources)/write_html_tail.F
	ifort -g -nus -w -c -o $(objects)/write_html_tail.o $(sources)/write_html_tail.F

$(objects)/wmo_search.o: $(sources)/wmo_search.F
	ifort -g -nus -w -c -o $(objects)/wmo_search.o $(sources)/wmo_search.F

$(objects)/write_abbv_details.o: $(sources)/write_abbv_details.F
	ifort -g -nus -w -c -o $(objects)/write_abbv_details.o $(sources)/write_abbv_details.F
	
$(objects)/check_pattern.o: $(sources)/check_pattern.F
	ifort -g -nus -w -c -o $(objects)/check_pattern.o $(sources)/check_pattern.F

$(objects)/write_name_details.o: $(sources)/write_name_details.F
	ifort -g -nus -w -c -o $(objects)/write_name_details.o $(sources)/write_name_details.F

# Now set up to clean up .obj files
clean:
	rm $(objects)/*.o $(exes)/*.exe






