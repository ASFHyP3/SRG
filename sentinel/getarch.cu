//!!!!!!!!!!!!!!
//!
//!   getarch - get gpu/cpu architecture of machine
//!
//!!!!!!!!!!!!!!

#include <stdlib.h>
#include <stdio.h>
#include <cuda.h>
#include <omp.h>

extern "C" void getarch_(long *numgpus, long *numcpus)
{

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
  *numgpus=nDevices;
  *numcpus=omp_get_num_procs();
}

