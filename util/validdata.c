/*  does a file contain invalid data?  */

#include <stdio.h>
#include <math.h>

main(int argc, char *argv[]){

  FILE *fp;
  float *chunk;
  long nbytes, nfloats, ierr, i, j, npts, bufsize, nchunks, extra;
  char *filename;

  long row, col;

  if(argc<2){
    fprintf(stderr,"usage: %s filename\n",argv[0]);
    fprintf(stderr,"file is assumed to be floats\n",argv[0]);
    exit(1);
  }
    /* get file name */
    filename=argv[1];
    /* open input file */
     if((fp=fopen(filename,"r"))==NULL){
	fprintf(stderr,"unable to open file %s\n",filename);
	exit(1);
      }
     /* size of file */
     nbytes=fseek(fp, 0, SEEK_END);
     nbytes=ftell(fp);
     printf("File length, bytes: %ld\n",nbytes);
     nfloats=nbytes/4;

     npts=10000000;
     bufsize=npts*4;
     nchunks=nfloats/npts;
     extra=nbytes-nchunks*bufsize;
     printf("Using %d chunks of data, %d extra bytes.\n",nchunks,extra);

     /* allocate a buffer for data */
     if((chunk=malloc(bufsize*sizeof(unsigned char)))==NULL){
       fprintf(stderr,"Out of memory.\n");
       exit(1);
     }
     rewind(fp);

     for(j=0;j<nchunks;j++){
       ierr=fread(chunk,sizeof(unsigned char),bufsize,fp);
       /*       printf("Bytes read: %d\n",ierr);  */

       for(i=0;i<npts;i++){
	 if(isnan(chunk[i]) || isinf(chunk[i])){
	   printf("Nan or infinite data at float number %d\n",i+j*npts);
	 }
       }
     }
     ierr=fread(chunk,sizeof(unsigned char),extra,fp);
     /*     printf("Bytes read: %d\n",ierr);  */

     for(i=0;i<extra/4;i++){
       if(isnan(chunk[i]) || isinf(chunk[i])){
	 printf("Nan or infinite data at float number %d\n",i+j*npts);
       }
     }

}
