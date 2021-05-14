//!!!!!!!!!!!!!!
//!
//!  testmultigpu - test using multiple gpus
//!
//!!!!!!!!!!!!!!

#include <stdlib.h>
#include <stdio.h>
#include <complex.h>
#include <math.h>
#include <unistd.h>
#include <cuComplex.h>
#include <cuda.h>
#include <cmath>
#include <omp.h>

int main()
{

//  get some basic gpu architecture info
  int blockSize = 256;
  int numBlocks = (1000+blockSize-1)/blockSize;
  printf("GPU blocksize, numblocks: %d %d\n",blockSize,numBlocks);

// get the configuration of the machine
  int nDevices, iDevice;  // multigpu handling parameters
  cudaGetDeviceCount(&nDevices);
  printf("Number of gpus: %d\n",nDevices);
  printf("Number of cpus: %d\n",omp_get_num_procs());
  for(iDevice=0;iDevice<nDevices;iDevice++){
    cudaDeviceProp dprop;
    cudaGetDeviceProperties(&dprop,iDevice);
    printf(" %d %s\n",iDevice,dprop.name);
  }

  //  use one cpu thread per device
  omp_set_num_threads(nDevices);

// parallel section
  omp_set_num_threads(nDevices);
#pragma omp parallel
  {
  unsigned int cpu_thread_id = omp_get_thread_num();  // which thread 
  unsigned int num_cpu_threads = omp_get_num_threads();

  // set and check the CUDA device for this CPU thread
  int gpu_id = -1;
  cudaSetDevice(cpu_thread_id % nDevices);        // "% nDevices" allows more CPU threads than GPU devices
  cudaGetDevice(&gpu_id);
 
  printf("CPU thread %d (of %d) uses CUDA device %d\n", cpu_thread_id, num_cpu_threads, gpu_id);
  
  } // end parallel section

}

