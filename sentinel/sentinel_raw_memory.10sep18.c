// decode raw sentinel data product
//   write into swath-sorted files
//   use memory mapped io for speed
//   parallelize decoding of each line for multicore processors

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <math.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <omp.h>

// function protoypes for external functions
long long int filelen_(char* filename); // define our filelength routine

// several functions used in this code
int int2(unsigned char *data){
  return data[1]+256*data[0];
}
int int4(unsigned char *data){
  long temp[1];
  unsigned char tmp,d[4];
  memcpy(d,data,4);
  tmp=d[0];d[0]=d[3];d[3]=tmp;
  tmp=d[2];d[2]=d[1];d[1]=tmp;
  memcpy(temp,d,4);
  return temp[0];
}
unsigned int3(unsigned char *data){
  return data[2]+data[1]*256+data[0]*256*256;
}
long long int8(unsigned char *data){
  long long temp[1];
  unsigned char tmp, d[8];
  memcpy(d,data,8);
  tmp=d[0];d[0]=d[7];d[7]=tmp;
  tmp=d[1];d[1]=d[6];d[6]=tmp;
  tmp=d[2];d[2]=d[5];d[5]=tmp;
  tmp=d[3];d[3]=d[4];d[4]=tmp; 
  memcpy(temp,d,8);
  return temp[0];
}
double double8(unsigned char *data){
  double temp[1];
  unsigned char tmp, d[8];
  memcpy(d,data,8);
  tmp=d[0];d[0]=d[7];d[7]=tmp;
  tmp=d[1];d[1]=d[6];d[6]=tmp;
  tmp=d[2];d[2]=d[5];d[5]=tmp;
  tmp=d[3];d[3]=d[4];d[4]=tmp; 
  memcpy(temp,d,8);
  return temp[0];
}

double sampleFrequency(int rangeDecimation){
  double fref;

  fref=37.53472224;

  switch (rangeDecimation){
  case 0:
    return 3./4.*4.*fref;
    break;
  case 1:
    return 2./3.*4.*fref;
    break;
  case 3:
    return 5./9.*4.*fref;
    break;
  case 4:
    return 4./9.*4.*fref;
    break;
  case 5:
    return 3./8.*4.*fref;
    break;
  case 6:
    return 1./3.*4.*fref;
    break;
  case 7:
    return 1./6.*4.*fref;
    break;
  case 8:
    return 3./7.*4.*fref;
    break;
  case 9:
    return 5./16.*4.*fref;
    break;
  case 10:
    return 3./26.*4.*fref;
    break;
  case 11:
    return 4./11.*4.*fref;
    break;
  }
}

int main (int argc, char *argv[]) {

  unsigned char d[36], da[26], scandata[100000];;
  //float cpxsamps[65536];
  //float *cpxsamps = (float *)malloc(65536*sizeof(float));

  FILE *fp, *fpout, *fplength, *fpswath, *fptimes;
  FILE *fpout1, *fpout2, *fpout3;
  char *filename;
  int i, j, recs, recsindex, nthreads;
  unsigned char tmp;
  unsigned int Days, Millisecs, Microsecs, packetLength, frames;
  double dateAndTime, deltaTime;
  unsigned int deltaSize, dataUnitsOffset;
  long long byteOffset;
  unsigned char variableSizeFlag;
  unsigned int packetId,packetSequenceControl,packetDataLength;
  float zeros[30000*2];
  double times[100000], time[1];

  unsigned char *rawdata;
  unsigned char *swath10,*swath11,*swath12;

  long long *iptr;
  long *line10, *line11, *line12, nswath[3];
  //unsigned char *outline = (unsigned char *)malloc(8*30000*sizeof(unsigned char));
  //unsigned char *data = (unsigned char *)malloc(100000*sizeof(unsigned char));

  struct stat sb;
  char fname[200];
  char basename[200];
  char *charerr;

  if(argc<2){
    fprintf(stderr,"usage: %s base_filename (without .dat)\n",
            argv[0]);
    exit(1);
  }

  // array for padding output to constant length
  for(i=0;i<30000*2;i++)zeros[i]=0.0;

  // annot.dat file
  charerr = strcpy(basename,argv[1]);
  //printf("opening files with base name: %s\n",basename);
  charerr=strcpy(fname,basename);
  charerr = strcat(fname,"-annot.dat");
  //  printf("annotation file %s\n",fname);

  i=stat(fname, &sb);
  recs=sb.st_size/26;
  printf("annotation file size, records: %d %d\n",sb.st_size,recs);

  fp=fopen(fname,"r");
  //  fptimes=fopen("times","w");

  for (i=0;i<recs;i++){
    fread(&da,1,26,fp);
    // sensing time
    Days=int2(&da[0]);
    Millisecs=int4(&da[2]);
    Microsecs=int2(&da[6]);
    //    printf("sensing time %d %d %d\n",Days,Millisecs,Microsecs);
    //    fprintf(fptimes,"%d %f\n",i,Millisecs/1000.+Microsecs/1000000.);
    times[i]=Millisecs/1000.+Microsecs/1000000.;
    Days=int2(&da[8]);
    Millisecs=int4(&da[10]);
    Microsecs=int2(&da[14]);
    //printf("downlink time %d %d %d\n",Days,Millisecs,Microsecs);
    packetLength=int2(&da[16]);
    frames=int2(&da[18]);
    //printf("packet length, frames %d %d\n",packetLength,frames);
  }
  fclose(fp);
  //  fclose(fptimes);
  
  // index.dat file
  charerr=strcpy(fname,basename);
  charerr = strcat(fname,"-index.dat");
  //  printf("index file %s\n",fname);
  i=stat(fname, &sb);
  recsindex=sb.st_size/36;

  fp=fopen(fname,"r");
  printf("index file size, records: %d %d\n",sb.st_size,recsindex);

  for (i=0;i<5;i++){
    fread(&d,1,36,fp);
    dateAndTime=double8(&d[0]);
    deltaTime=double8(&d[8]);
    deltaSize=int4(&d[16]);
    dataUnitsOffset=int4(&d[20]);
    byteOffset=int8(&d[24]);
    variableSizeFlag=d[32];
    //printf("%f %f %d %d %d %d\n",dateAndTime,deltaTime,deltaSize,dataUnitsOffset,byteOffset,variableSizeFlag);
  }
  fclose(fp);
  
  // measurement data file

  long coarseTime,sync;
  double fineTime,fref;
  int rangeDecimation,nq;
  double pri,swst,swl,rxGain,rampRate,startFreq,pulseLength;
  int baqMode, baqBlockLength, rank, swath, elevationbeam, sigtyp;
  double sampleFreq;
  unsigned int polarity;
  char *map,*map1,*map2,*map3;  // byte arrays for memory maps
  int fd, fdout1, fdout2, fdout3;
  long long int filesize,filesize1,filesize2,filesize3;
  long long int result;

  fref=37.53472224;

  charerr=strcpy(fname,basename);
  charerr=strcat(fname,".dat");
  i=stat(fname, &sb);
  // memory map the input file
    fd = open(fname, O_RDONLY);
    if (fd == -1) {
        perror("Error opening file for reading");
        exit(EXIT_FAILURE);
    }
    // get the file size
    filesize=filelen_(fname);
    printf("Input file size, bytes: %lld\n",filesize);

    map = mmap(0, filesize, PROT_READ, MAP_SHARED, fd, 0);
    if (map == MAP_FAILED) {
        close(fd);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }

    //  fp=fopen(fname,"r");
    //  printf("data file size : %d\n\n",sb.st_size);

  //  fpout=fopen("raw.dat","w");
  fpout1=fopen("raw.1.dat","w");
  fpout2=fopen("raw.2.dat","w");
  fpout3=fopen("raw.3.dat","w");
  //  fpswath=fopen("swath.txt","w");

  // allocate memory for raw data
  //  rawdata = (unsigned char *)malloc(sb.st_size*sizeof(unsigned char));
  iptr = (long long *)malloc((recs+1)*sizeof(long long));
  line10 = (long *)malloc((recs+1)*sizeof(long));
  line11 = (long *)malloc((recs+1)*sizeof(long));
  line12 = (long *)malloc((recs+1)*sizeof(long));

  // and read it in
  //fread(rawdata,1,sb.st_size,fp);
  //printf("Read in %d bytes\n",sb.st_size);
  //fclose(fp);

  // scan for line pointers
  nswath[0]=0;nswath[1]=0;nswath[2]=0;
  iptr[0]=0;
  for (i=0;i<recs;i++){
    memcpy(scandata,&map[iptr[i]],68);
    packetDataLength=int2(&scandata[4])+1;
    swath=scandata[64];
    sigtyp=(scandata[63]&0xf0)/16;
    iptr[i+1]=iptr[i]+packetDataLength+6;
    // how big is each swath file?
    // and what is the linecounter for each?
    if(sigtyp==0){
      if(swath==10)line10[i]=nswath[0];
      if(swath==11)line11[i]=nswath[1];
      if(swath==12)line12[i]=nswath[2];
      line10[i]=nswath[0];
      line11[i]=nswath[1];
      line12[i]=nswath[2];
      nswath[swath-10]++;
    }
  }
  printf("%d offset pointers computed, end ptr= %d\n",recs,iptr[recs]);
  printf("Records in each swath: %d %d %d\n",nswath[0],nswath[1],nswath[2]);

    // memory map the output files - do this three times
    fdout1 = open("raw.1.dat", O_RDWR | O_CREAT | O_TRUNC, (mode_t)0600);
    if (fdout1 == -1) {
        perror("Error opening file for writing");
        exit(EXIT_FAILURE);
    }
    // Stretch the file size to the size of the (mmapped) array of ints
    filesize1=(long long int)nswath[0] * (long long int) (30000*8);
    result = lseek(fdout1, filesize1-1, SEEK_SET);
    if (result == -1) {
        close(fdout1);
        perror("Error calling lseek() to 'stretch' the file");
        exit(EXIT_FAILURE);
    }
    // define file length by writing a null at the end
    result = write(fdout1, "", 1);
    if (result != 1) {
        close(fdout1);
        perror("Error writing last byte of the file");
        exit(EXIT_FAILURE);
    }
    // Now the output file is ready to be mmapped.
    map1 = mmap(0, filesize1, PROT_READ | PROT_WRITE, MAP_SHARED, fdout1, 0);
    if (map1 == MAP_FAILED) {
        close(fdout1);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }
    fdout2 = open("raw.2.dat", O_RDWR | O_CREAT | O_TRUNC, (mode_t)0600);
    if (fdout2 == -1) {
        perror("Error opening file for writing");
        exit(EXIT_FAILURE);
    }
    // Stretch the file size to the size of the (mmapped) array of ints
    filesize2=(long long int)nswath[1] * (long long int) (30000*8);
    result = lseek(fdout2, filesize2-1, SEEK_SET);
    if (result == -1) {
        close(fdout2);
        perror("Error calling lseek() to 'stretch' the file");
        exit(EXIT_FAILURE);
    }
    // define file length by writing a null at the end
    result = write(fdout2, "", 1);
    if (result != 1) {
        close(fdout2);
        perror("Error writing last byte of the file");
        exit(EXIT_FAILURE);
    }
    // Now the output file is ready to be mmapped.
    map2 = mmap(0, filesize2, PROT_READ | PROT_WRITE, MAP_SHARED, fdout2, 0);
    if (map2 == MAP_FAILED) {
        close(fdout2);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }
    fdout3 = open("raw.3.dat", O_RDWR | O_CREAT | O_TRUNC, (mode_t)0600);
    if (fdout3 == -1) {
        perror("Error opening file for writing");
        exit(EXIT_FAILURE);
    }
    // Stretch the file size to the size of the (mmapped) array of ints
    filesize3=(long long int)nswath[2] * (long long int) (30000*8);
    result = lseek(fdout3, filesize3-1, SEEK_SET);
    if (result == -1) {
        close(fdout3);
        perror("Error calling lseek() to 'stretch' the file");
        exit(EXIT_FAILURE);
    }
    // define file length by writing a null at the end
    result = write(fdout3, "", 1);
    if (result != 1) {
        close(fdout3);
        perror("Error writing last byte of the file");
        exit(EXIT_FAILURE);
    }
    // Now the output file is ready to be mmapped.
    map3 = mmap(0, filesize3, PROT_READ | PROT_WRITE, MAP_SHARED, fdout3, 0);
    if (map1 == MAP_FAILED) {
        close(fdout3);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }
    
  // set up "optimal" number of threads, this may be machine dependent
    nthreads = omp_get_max_threads();
    if (nthreads > 5)nthreads=5;
    omp_set_num_threads(nthreads);

#pragma omp parallel for shared(recs,iptr,map,times,zeros,line10,line11,line12) private(packetDataLength,sigtyp,swath,nq,j,time)
  for (i=0;i<recs;i++){
    // local arrays for omp loop
    unsigned char *data = (unsigned char *)malloc(100000*sizeof(unsigned char));
    float *cpxsamps;
    cpxsamps = (float *)malloc(65536*sizeof(float));
    //float cpxsamps[65536];
    unsigned char *outline = (unsigned char *)malloc(8*30000*sizeof(unsigned char));
 
    if(i%2000==0)printf("decoding line %d\n",i);
    // read header info for line
    memcpy(data,&map[iptr[i]],68);  // retrieve packet length, etc.
    packetDataLength=int2(&data[4])+1;
    sigtyp=(data[63]&0xf0)/16;
    swath=data[64];
    nq=int2(&data[65]);
    memcpy(data,&map[iptr[i]],6+packetDataLength); // now load full line
    // zero pad rest of input array
    for (j=packetDataLength+6;j<65536;j++){
      data[j]=0;
    }
    // sample decoding if signal type is echo data
    if(sigtyp==0){
      //printf("calling decode line %d\n",i);
      decode_line(&data[6+62],nq,packetDataLength,cpxsamps,i);
      //printf("decoded line %d\n",i);
      //printf("cpxsamps vals %f %f %f\n",cpxsamps[1000],cpxsamps[2000],cpxsamps[3000]);
      time[0]=times[i];
      // assemble output line
      memcpy(outline,data,68);
      memcpy(&outline[68],&times[i],8);
      memcpy(&outline[76],zeros,4);
      //memcpy(&outline[80],zeros,8*nq*2);
      memcpy(&outline[80],cpxsamps,8*nq*2);
      memcpy(&outline[80+8*nq*2],zeros,8*(30000-nq*2-80/8));
      // sort by swath
      for(j=0; j<30000*8;j++){
	if(swath==10)map1[(long long int)line10[i]*(long long int)30000*8+j]=outline[j];
	if(swath==11)map2[(long long int)line11[i]*(long long int)30000*8+j]=outline[j];
	if(swath==12)map3[(long long int)line12[i]*(long long int)30000*8+j]=outline[j];
      }
    } // end sigtyp if block
     free(data);
     free(cpxsamps);
     free(outline);
   } // end i loop over nrecs
  //  }  // end parallel section
  fclose(fpout1);
  fclose(fpout2);
  fclose(fpout3);

    // unmap input file
    if (munmap(map, filesize) == -1) {
        perror("Error un-mmapping the file");
    }
    close(fd);
    // unmap output files
    if (munmap(map1, filesize1) == -1) {
        perror("Error un-mmapping the output file");
    }
    close(fdout1);
    if (munmap(map2, filesize2) == -1) {
        perror("Error un-mmapping the output file");
    }
    close(fdout2);
    if (munmap(map3, filesize3) == -1) {
        perror("Error un-mmapping the output file");
    }
    close(fdout3);

}

