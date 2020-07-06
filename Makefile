#
#  Project files
#

OBJDIR = 	obj
EXPORTDIR = 	exportsrc
LIBPARSE =	libparse.a
BINCHECK =	check
BINSPEED = 	speed
EXPORT =	exportsource
MODSRC =	params.f90 \
		extrafunc.f90 \
		interpreter.f90
BINSRC = 	check.f90
SPEEDSRC =	speed.f90
MODOBJ =	$(MODSRC:%.f90=$(OBJDIR)/%.o)
BINOBJ =	$(BINSRC:%.f90=$(OBJDIR)/%.o)
DIRS =		$(OBJDIR)

#
#  Compile options
#

#
# gfortran MacOSX
#
F90COMPILER =	gfortran
F90FLAGS =	-g -cpp -fbounds-check -fstack-check -pedantic-errors -Wuninitialized
#F90FLAGS =      -cpp -O3 -msse3 -ffast-math -floop-optimize -fprefetch-loop-arrays -funroll-loops -funsafe-math-optimizations
F90MOD =	-I./$(OBJDIR) -J./$(OBJDIR)
F90LIBS =	-L. -lparse

#
# NAG MacOSX
#
#F90COMPILER = 	f95
#F90FLAGS =	-kind=byte -O4 -Ounsafe -fpp
#F90FLAGS = 	-kind=byte -C -g -fpp -nan
#F90MOD = 	-I./$(OBJDIR) -mdir ./$(OBJDIR)
#F90LIBS = 	-L. -lparse

#
# Linux Alpha
#
#F90COMPILER =   fort
##F90FLAGS =      -g -C -fpp
#F90FLAGS =      -fast -fpp -speculate all
#F90MOD =        -I./$(OBJDIR) -module ./$(OBJDIR)
#F90LIBS =       -L. -lparse

#
# Tru64 Alpha
#
#F90COMPILER =   f90
##F90FLAGS =      -g -C -cpp
#F90FLAGS =      -fast -cpp -speculate all
#F90MOD =        -module ./$(OBJDIR)
#F90LIBS =       -L. -lparse

#
# Intel x86
#
#F90COMPILER =   ifort
#F90FLAGS =      -g -fpp -check all -debug full
#F90FLAGS =      -O3 -ip -static -fpp
#F90MOD =        -I ./$(OBJDIR) -module ./$(OBJDIR)
#F90LIBS =       -L. -lparse


#
#
#

CPP = 		/usr/bin/cpp -C -P -traditional
AR = 		/usr/bin/ar rcs
F90DEFINES = 	#-DSYSBESSEL -DSYSGAMMA -DSYSLGAMMA -DSYSERF -DDEBUG

#
#  Make options
#

bin:			$(BINCHECK)
lib:			$(LIBPARSE)
export:			$(EXPORT)

#
#  Dependancies
#

$(OBJDIR)/params.o : 	Makefile \
			params.f90

$(OBJDIR)/extrafunc.o :	Makefile \
			$(OBJDIR)/params.o \
			extrafunc.f90

$(OBJDIR)/interpreter.o : \
			Makefile \
			$(OBJDIR)/params.o \
			$(OBJDIR)/extrafunc.o \
			interpreter.f90

$(OBJDIR)/check.o :	$(LIBPARSE) \
			Makefile \
			check.f90

$(OBJDIR)/speed.o :	Makefile \
			speed.f90

#
#  Build options
#

$(OBJDIR)/%.o : 	%.f90
	$(F90COMPILER) -c $(F90FLAGS) $(F90MOD) $(F90DEFINES) $< -o $@

$(LIBPARSE) :		$(DIRS) \
			$(MODOBJ)
	$(AR) $(LIBPARSE) $(MODOBJ)

$(BINCHECK) :		$(DIRS) \
			$(LIBPARSE) \
			$(BINOBJ)
	$(F90COMPILER) $(F90FLAGS) $(F90DEFINES) $(BINOBJ) -o $(BINCHECK) $(F90LIBS)

$(EXPORT) :		$(EXPORTDIR)
	for srcfile in $(MODSRC) $(BINSRC) ; do $(CPP) $(F90DEFINES) $${srcfile} > $(EXPORTDIR)/$${srcfile} ; done
	cp TOUSE README $(EXPORTDIR)

$(BINSPEED) :		$(DIRS) \
			$(MODSRC) \
			$(SPEEDSRC)
	$(F90COMPILER) $(F90FLAGS) $(F90DEFINES) $(F90MOD) $(MODSRC) $(SPEEDSRC) -o $(BINSPEED)

#
#  Create directories
#

$(DIRS) :
	if ( test ! -d $@ ) then ( mkdir $@ ) fi

$(EXPORTDIR) :
	if ( test ! -d $@ ) then ( mkdir $@ ) fi

#
#  Clean up
#

clean:
	rm -f $(OBJDIR)/*
	rm -f $(LIBPARSE)
	rm -f $(BINCHECK) $(BINSPEED)
