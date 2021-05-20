CLIENT = ampcor
#FFLAGS = -c -O3 -ffixed-line-length-none 
FFLAGS = -c -O3 -ffixed-line-length-none -dalign -fopenmp 
FFLAGS1 = -c -O3 -ffixed-line-length-none

CLIENTOBJS = ampcor.o correlate.o second.o

CLIENTLIBS = /home/zebker/roi_pac_3/ROI_PAC_3_0/ROI_PAC/multibuild-071009-1328/gfortran64/LIB/src/libroipac.a -fopenmp -lfftw3f
HOST_F77 = gfortran
HOST_CC = gcc
CFLAGS = -c

.SUFFIXES:  .f90 .f

clean:
	rm -f *.o
	rm -f ampcor

$(CLIENT):  $(CLIENTOBJS)
	$(HOST_F77) $(CLIENTOBJS) -o $(CLIENT) $(CLIENTLIBS)

.f90.o: 
	$(HOST_F77) $(FFLAGS) $<

.f.o: 
	$(HOST_F77) $(FFLAGS1) $<

.c.o:   
	$(HOST_CC) $(CFLAGS) $<
