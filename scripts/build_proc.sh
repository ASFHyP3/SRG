#!/bin/bash

# Keeping these lines here in case we need to switch back to grabbing the FFTW location
# dynamically again
# MULTIARCH_DIR=/usr/lib/$(gcc -print-multiarch)
# FFTW_LIB=$MULTIARCH_DIR/libfftw3f.a
echo 'using FFTW library:' $FFTW_LIB
if [[ "$USEGPU" == "true" ]]; then
    nvcc -o gpu_arch gpu_arch.cu
    echo 'building with GPU support, capability version' $GPU_ARCH
fi

cd DEM
gfortran -o mosaicDEM mosaicDEM.f90
gfortran -o createspecialdem createspecialdem.f90
gfortran -o geoid2008_ellipsoid_interpolate geoid2008_ellipsoid_interpolate.f90
cd ..

cd sentinel
gfortran -o createslc createslc.f90
gcc -c azimuth_compress_cpu.c -lm -fopenmp
gcc -c filelen.c io.c sentinel_raw_process_cpu.c decode_line_memory.c -lm -fopenmp
gfortran -c processsubcpu.f90 backprojectcpusub.f90 bounds.f90 orbitrangetime.f90 latlon.f90 intp_orbit.f90 radar_to_xyz.f90 unitvec.f90 tcnbasis.f90 curvature.f90 cross.f90 orbithermite.f sentineltimingsub.f90 getburststatevectors.f90 -ffixed-line-length-none -fopenmp
gcc -o sentinel_raw_process_cpu sentinel_raw_process_cpu.o decode_line_memory.o processsubcpu.o backprojectcpusub.o azimuth_compress_cpu.o bounds.o orbitrangetime.o latlon.o intp_orbit.o radar_to_xyz.o unitvec.o tcnbasis.o curvature.o cross.o orbithermite.o filelen.o io.o sentineltimingsub.o getburststatevectors.o $FFTW_LIB -lgfortran -lgomp -lm -lrt -lpthread
echo 'built sentinel_raw_process_cpu'

cd geo2rdr
gfortran -o estimatebaseline estimatebaseline.f90 intp_orbit.f90 latlon.f90 orbithermite.f -ffixed-line-length-none

echo 'finished with sentinel directory'

cd ../..
cd util
gcc -c io.c
gfortran -o nbymi2 nbymi2.f io.o
gfortran -c lsq.f90
gfortran -o regressheight regressheight.f90 lsq.o
gfortran -o mergeslcs mergeslcs.f90

echo 'finished with util directory'

cd ..
cd int
gfortran -o findrefpoints findrefpoints.f90
gfortran -o refpointsfromsim refpointsfromsim.f90

echo 'built findrefpoints'

gcc -c ../util/filelen.c

echo 'compiled filelen'

gfortran -o crossmul crossmul.f90 filelen.o $FFTW_LIB -fopenmp -lrt -lpthread

echo 'built crossmul'

gfortran -o makecc makecc.f90 filelen.o -fopenmp -lrt -lpthread

echo 'finished with int directory'

cd ..
cd sbas
gfortran -o sbas sbas.f90 svd.f90 -fopenmp -lrt -lpthread
echo 'built sbas in sbas directory'

cd ..
cd ps
gfortran -o cosine_sim cosine_sim.f90 -fopenmp $FFTW_LIB
gfortran -o psinterp psinterp.f90 -fopenmp
echo 'Built cosine_sim and psinterp in ps directory'

cd ..
cd snaphu_v2.0b0.0.0/src
make CFLAGS=-O3 -s

cd ..
if [ -e $PROC_HOME/bin ]; then echo "copying snaphu"; else mkdir $PROC_HOME/bin; fi
cp bin/snaphu $PROC_HOME/bin/snaphu
cd ..

echo 'built snaphu'

cd sentinel

gcc -c filelen.c io.c sentinel_raw_process.c decode_line_memory.c -lm -fopenmp

echo 'built raw_process components in sentinel'

gfortran -c processsub.f90 backprojectgpusub.f90 bounds.f90 orbitrangetime.f90 latlon.f90 intp_orbit.f90 radar_to_xyz.f90 unitvec.f90 tcnbasis.f90 curvature.f90 cross.f90 orbithermite.f sentineltimingsub.f90 getburststatevectors.f90 -ffixed-line-length-none -fopenmp

if [[ "$USEGPU" == "true" ]]; then
    nvcc -o howmanygpus howmanygpus.cu
    nvcc -gencode arch=compute_$GPU_ARCH,code=sm_$GPU_ARCH -c azimuth_compress.cu -Wno-deprecated-gpu-targets
    nvcc -gencode arch=compute_$GPU_ARCH,code=sm_$GPU_ARCH -o sentinel_raw_process sentinel_raw_process.o decode_line_memory.o processsub.o backprojectgpusub.o azimuth_compress.o bounds.o orbitrangetime.o latlon.o intp_orbit.o radar_to_xyz.o unitvec.o tcnbasis.o curvature.o cross.o orbithermite.o filelen.o io.o sentineltimingsub.o getburststatevectors.o $FFTW_LIB -lstdc++ -lgfortran -lgomp
    echo 'built gpu components components in sentinel'
fi

cd ..
