/*  find max and min in a file  */

#include <stdio.h>
#include <math.h>

main(int argc, char *argv[]){

  FILE *fp;
  float *chunk;
  long nbytes, nfloats, ierr, i;
  char *filename;
  double qmin, qmax;

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
     printf("File length, bytes: %d\n",nbytes);
     nfloats=nbytes/4;

     /* read it all into memory (I know this is lame) */
     if((chunk=malloc(nbytes*sizeof(unsigned char)))==NULL){
       fprintf(stderr,"Out of memory.\n");
       exit(1);
     }
     rewind(fp);
     ierr=fread(chunk,sizeof(unsigned char),nbytes,fp);
     printf("Bytes read: %d\n",ierr);
     ierr=fclose(fp);

     /* check */
     qmax=chunk[0];
     qmin=chunk[0];
     for(i=0;i<nfloats;i++){
       if(isnan(chunk[i]) || isinf(chunk[i])){
	 printf("Nan or infinite data at float number %d\n",i);
	 chunk[i]=0.;
       }
       if(chunk[i]<qmin)qmin=chunk[i];
       if(chunk[i]>qmax)qmax=chunk[i];
     }
     printf("Max, min are: %f %f \n",qmax,qmin);
}
