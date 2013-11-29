# Makefile created by mkmf $Id: mkmf,v 14.0 2007/03/20 22:13:27 fms Exp $ 

include /home/pierre/DEV/bin/gfortran_args


.DEFAULT:
	-touch $@
all: access_data
access_data.o: ./access_data.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./access_data.f90
SRC = ./access_data.f90
OBJ = access_data.o
clean: neat
	-rm -f .cppdefs $(OBJ) access_data
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
access_data: $(OBJ) 
	$(LD) $(OBJ) -o access_data  $(LDFLAGS)
