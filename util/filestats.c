#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

long long filestats_(char *filename){

  long long buf[20];
  struct stat sb;
  int ierr,i;
  char fname[200];

  memcpy(fname,filename,200); /* file name into array */

  /*  null terminate file name */
  for(i=0;i<200;i++){
    if(fname[i]==32)fname[i]=0; 
    if(fname[i]==0)break;
  }

  ierr = stat(fname, &sb);
  ierr = stat(fname, &buf);
  for(i=0; i<20; i++)printf("buf: %lld\n",buf[i]);  

  printf("I-node number:            %ld\n", (long) sb.st_ino);
  printf("Mode:                     %lo (octal)\n",(unsigned long) sb.st_mode);
  printf("Link count:               %ld\n", (long) sb.st_nlink);
  printf("Ownership:                UID=%ld   GID=%ld\n",(long) sb.st_uid, (long) sb.st_gid);
  printf("Preferred I/O block size: %ld bytes\n", (long) sb.st_blksize);
  printf("File size:                %lld bytes\n", (long long) sb.st_size);
  printf("Blocks allocated:         %lld\n", (long long) sb.st_blocks);

  return sb.st_size;
}
