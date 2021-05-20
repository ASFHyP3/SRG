// create a zero-filled slc file

#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

int main (int argc, char *argv[]) {

  int fdout, xsize, ysize, result;
  long long int size, i;
  char *map, *ptr;

  if(argc<3){
    fprintf(stderr,"usage: %s pixels lines\n",argv[0]);
    exit(1);
  }

  // input size
  xsize=strtol(argv[1],&ptr,10);
  ysize=strtol(argv[2],&ptr,10);
  size=(long long int)xsize * (long long int)ysize * (long long int)8;

  //printf("%d %d %lld\n",xsize,ysize,size);

    fdout = open("slc", O_RDWR | O_CREAT | O_TRUNC, (mode_t)0600);
    if (fdout == -1) {
        perror("Error opening file for writing");
        exit(EXIT_FAILURE);
    }
    // Stretch the file size to the size of the (mmapped) array of ints
    result = lseek(fdout, size-1, SEEK_SET);
    if (result == -1) {
        close(fdout);
        perror("Error calling lseek() to 'stretch' the file");
        exit(EXIT_FAILURE);
    }
    // define file length by writing a null at the end
    result = write(fdout, "", 1);
    if (result != 1) {
        close(fdout);
        perror("Error writing last byte of the file");
        exit(EXIT_FAILURE);
    }
    // Now the output file is ready to be mmapped.
    map = mmap(0, size, PROT_READ | PROT_WRITE, MAP_SHARED, fdout, 0);
    if (map == MAP_FAILED) {
        close(fdout);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }

    // write zeros
    for(i=0; i<size; i++)map[i]=0;
    // unmap the file
    if (munmap(map, size) == -1) {
        perror("Error un-mmapping the output file");
    }
    close(fdout);
}
