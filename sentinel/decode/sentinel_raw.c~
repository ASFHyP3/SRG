// decode raw sentinel data product
//   write into swath-sorted files
//   no memory mapping
//   parallelize decoding of each line for multicore processors

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <math.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <omp.h>

// some declarations
void decode_line(unsigned char *data, int nq, int packetDataLength, float *samples, int linenum);

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

  FILE *fp;
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

  // set the input file name to measurement data file
  charerr = strcpy(basename,argv[1]);
  charerr=strcpy(fname,basename);

  // annot.dat file -- we apparently need this as the actuakl times are not in the measurement file
  charerr = strcat(fname,"-annot.dat");

  i=stat(fname, &sb);
  recs=sb.st_size/26;
  printf("annotation file size, records: %ld %d\n",sb.st_size,recs);

  fp=fopen(fname,"r");

  for (i=0;i<recs;i++){
    fread(&da,1,26,fp);
    // sensing time
    Days=int2(&da[0]);
    Millisecs=int4(&da[2]);
    Microsecs=int2(&da[6]);
    times[i]=Millisecs/1000.+Microsecs/1000000.;
    Days=int2(&da[8]);
    Millisecs=int4(&da[10]);
    Microsecs=int2(&da[14]);
    packetLength=int2(&da[16]);
    frames=int2(&da[18]);
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
  unsigned char *map;  // byte array for input data
  FILE  *fd, *fdout1, *fdout2, *fdout3;
  long long int filesize,filesize1,filesize2,filesize3;
  long long int result;

  fref=37.53472224;

  charerr=strcpy(fname,basename);
  charerr=strcat(fname,".dat");
  // get the file size
  i=stat(fname, &sb);
  filesize=sb.st_size;
  printf("Input file size, bytes: %lld\n",filesize);

  // read input file to memory
  map = (unsigned char *)malloc(filesize);
  fd=fopen(fname,"r");
  fread(map,1,filesize,fd);
  fclose(fd);
    
  // allocate memory for raw data pointers
  iptr = (long long *)malloc((recs+1)*sizeof(long long));
  line10 = (long *)malloc((recs+1)*sizeof(long));
  line11 = (long *)malloc((recs+1)*sizeof(long));
  line12 = (long *)malloc((recs+1)*sizeof(long));

  // scan for line pointers
  nswath[0]=0;nswath[1]=0;nswath[2]=0;
  iptr[0]=0;
  recs=0;
  while(iptr[recs]<filesize){ 
    memcpy(scandata,&map[iptr[recs]],68);
    packetDataLength=int2(&scandata[4])+1;
    swath=scandata[64];
    sigtyp=(scandata[63]&0xf0)/16;
    iptr[recs+1]=iptr[recs]+packetDataLength+6;
    // how big is each swath file?
    // and what is the linecounter for each?
    if(sigtyp==0){
      if(swath==10)line10[recs]=nswath[0];
      if(swath==11)line11[recs]=nswath[1];
      if(swath==12)line12[recs]=nswath[2];
      line10[recs]=nswath[0];
      line11[recs]=nswath[1];
      line12[recs]=nswath[2];
      nswath[swath-10]++;
    }
    recs++;
  }
  printf("%d offset pointers computed, end ptr= %lld\n",recs,iptr[recs]);
  printf("Records in each swath: %ld %ld %ld\n",nswath[0],nswath[1],nswath[2]);

    //  open shadow file writes
    fdout1 = fopen("raw.1.dat","w");
    fdout2 = fopen("raw.2.dat","w");
    fdout3 = fopen("raw.3.dat","w");
    
    // set up "optimal" number of threads, this may be machine dependent
    nthreads = omp_get_max_threads();
    //    if (nthreads > 5)nthreads=5;
    //    omp_set_num_threads(nthreads);

    //    printf("lines in raw files %d %d %d\n",line10[recs-1],line11[recs-1],line12[recs-1]);
    // allocate raw file memory areas
    unsigned char *raw1, *raw2, *raw3;
    raw1=(unsigned char *)malloc((long long int)nswath[0]*(long long int)240000*sizeof(unsigned char));
    raw2=(unsigned char *)malloc((long long int)nswath[1]*(long long int)240000*sizeof(unsigned char));
    raw3=(unsigned char *)malloc((long long int)nswath[2]*(long long int)240000*sizeof(unsigned char));
    
#pragma omp parallel for shared(recs,iptr,map,times,zeros,line10,line11,line12,raw1,raw2,raw3) private(packetDataLength,sigtyp,swath,nq,j,time)
  for (i=0;i<recs;i++){
    // local arrays for omp loop
    unsigned char *data;
    data = (unsigned char *)malloc(100000*sizeof(unsigned char));
    float *cpxsamps;
    cpxsamps = (float *)malloc(65536*sizeof(float));
    unsigned char *outline;
    outline = (unsigned char *)malloc(8*30000*sizeof(unsigned char));
 
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
      decode_line(&data[6+62],nq,packetDataLength,cpxsamps,i);
      time[0]=times[i];
      // assemble output line
      memcpy(outline,data,68);
      memcpy(&outline[68],&times[i],8);
      memcpy(&outline[76],zeros,4);
      memcpy(&outline[80],cpxsamps,8*nq*2);
      memcpy(&outline[80+8*nq*2],zeros,8*(30000-nq*2-80/8));
      // sort by swath
      if(swath==10){
	memcpy(&raw1[(long long int)line10[i]*(long long int)30000*8],outline,30000*8);
      }
      if(swath==11){
	memcpy(&raw2[(long long int)line11[i]*(long long int)30000*8],outline,30000*8);
      }
      if(swath==12){
	memcpy(&raw3[(long long int)line12[i]*(long long int)30000*8],outline,30000*8);
      }
      
    } // end sigtyp if block
     free(data);
     free(cpxsamps);
     free(outline);
   } // end i loop over nrecs
  //  end parallel section

  fwrite(raw1,1,(long long int)nswath[0]*(long long int)30000*8,fdout1);
  fwrite(raw2,1,(long long int)nswath[1]*(long long int)30000*8,fdout2);
  fwrite(raw3,1,(long long int)nswath[2]*(long long int)30000*8,fdout3);
  
  fclose(fdout1);
  fclose(fdout2);
  fclose(fdout3);
}

