#include <stdio.h>

int main(void) {

  int nDevices;

  cudaGetDeviceCount(&nDevices);
  printf("%d\n",nDevices);
}
