#include <stdio.h>
#include <stdlib.h>

int main(void) {

    int dev=0;
    char str[100];

    cudaSetDevice(dev);
    cudaDeviceProp deviceProp;
    cudaGetDeviceProperties(&deviceProp, dev);
    // print architecture
    sprintf(str,"export GPU_ARCH=%d%d",deviceProp.major, deviceProp.minor);
    printf("%s\n",str);
}
