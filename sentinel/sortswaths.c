//
//  sortswaths - get the swath sorting information for a sentinel data set
//


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <math.h>

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

  unsigned char d[36], da[26], data[100000];
  float cpxsamps[65536];

  FILE *fp, *fpout, *fplength, *fpswath, *fptimes;
  char *filename;
  int i, j, recs, recsindex;
  unsigned char tmp;
  unsigned int Days, Millisecs, Microsecs, packetLength, frames;
  double dateAndTime, deltaTime;
  unsigned int deltaSize, dataUnitsOffset;
  long long byteOffset;
  unsigned char variableSizeFlag;
  unsigned int packetId,packetSequenceControl,packetDataLength;
  float zeros[30000*2];
  double times[100000], time[1];

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
  printf("opening files with base name: %s\n",basename);
  charerr=strcpy(fname,basename);
  charerr = strcat(fname,"-annot.dat");
  printf("annotation file %s\n",fname);

  i=stat(fname, &sb);
  recs=sb.st_size/26;
  printf("annotation file size, records: %d %d\n",sb.st_size,recs);

  fp=fopen(fname,"r");
  fptimes=fopen("times","w");

  for (i=0;i<recs;i++){
    fread(&da,1,26,fp);
    // sensing time
    Days=int2(&da[0]);
    Millisecs=int4(&da[2]);
    Microsecs=int2(&da[6]);
    //    printf("sensing time %d %d %d\n",Days,Millisecs,Microsecs);
    fprintf(fptimes,"%d %f\n",i,Millisecs/1000.+Microsecs/1000000.);
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
  fclose(fptimes);
  
  // index.dat file
  charerr=strcpy(fname,basename);
  charerr = strcat(fname,"-index.dat");
  printf("index file %s\n",fname);
  i=stat(fname, &sb);
  recsindex=sb.st_size/36;

  fp=fopen(fname,"r");
  printf("\nindex file size, records: %d %d\n",sb.st_size,recsindex);

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
  long iptr;
  double fineTime,fref;
  int rangeDecimation,nq;
  double pri,swst,swl,rxGain,rampRate,startFreq,pulseLength;
  int baqMode, baqBlockLength, rank, swath, elevationbeam, sigtyp;
  double sampleFreq;
  unsigned int polarity;

  fref=37.53472224;

  charerr=strcpy(fname,basename);
  charerr=strcat(fname,".dat");
  i=stat(fname, &sb);
  fp=fopen(fname,"r");
  printf("\ndata file size : %d\n\n",sb.st_size);

  //  fpout=fopen("raw.dat","w");
  fpswath=fopen("swath.txt","w");
  iptr=0;
  for (i=0;i<recs;i++){
    if(i%2000==0)printf("decoding line %d\n",i);
    // read header info for line
    fread(&data,1,6+62,fp); 
    packetId=int2(&data[0]);
    packetSequenceControl=int2(&data[2]);
    packetDataLength=int2(&data[4])+1;
    coarseTime=int4(&data[6]);
    fineTime=(int2(&data[10])+0.5)*pow(2.,-16.);
    sync=int4(&data[12]);
    baqMode=data[37]&31;
    baqBlockLength=8*(data[38]+1);
    rangeDecimation=data[40];
    sampleFreq=sampleFrequency(rangeDecimation);
    rxGain=data[41]*-0.5;
    polarity=(int2(&data[42])&32768)/32768;
    rampRate=pow(-1.,(1.-polarity))*(int2(&data[42])&32767)*fref*fref/pow(2.,21.);
    polarity=(int2(&data[44])&32768)/32768;
    startFreq=rampRate/4./fref+pow(-1.,(1.-polarity))*(int2(&data[44])&32767)*fref/pow(2.,14.);
    pulseLength=int3(&data[46])/fref;
    rank=data[49]&31;
    pri=int3(&data[50])/fref;
    swst=int3(&data[53])/fref;
    swl=int3(&data[56])/fref;
    elevationbeam=(data[60]&0xf0)/16;
    sigtyp=(data[63]&0xf0)/16;
    swath=data[64];
    nq=int2(&data[65]);
    fprintf(fpswath,"%d %d %d %d %d %d\n",i,elevationbeam,swath,nq,packetDataLength,iptr);
    // read the rest of the data record
   fread(&data[6+62],1,packetDataLength-62,fp);
   // update pointer
   iptr+=6;
   iptr+=packetDataLength;
  }
}

