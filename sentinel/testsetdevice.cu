//!!!!!!!!!!!!!!
//!
//!  testsetdevice command
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
  for(iDevice=0;iDevice<nDevices;iDevice++){
    cudaDeviceProp dprop;
    cudaGetDeviceProperties(&dprop,iDevice);
    printf(" %d %s\n",iDevice,dprop.name);
  }

  long int ret = cudaGetDevice(0);
 
  printf("setdevice returns %d\n", ret);
  

}

