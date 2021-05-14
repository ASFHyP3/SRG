// decode raw sentinel data product
//   write into swath-sorted files
//   read input file into memory for paralleization
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

  FILE *fp, *fpout, *fplength, *fpswath, *fptimes, *fpoutq;
  FILE *fpout1, *fpout2, *fpout3;
  char *filename;
  int i, j, recs, recsindex, bytecount;
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
  unsigned char *outline;

  long long *iptr;
  long *line10, *line11, *line12;

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
  //printf("annotation file %s\n",fname);

  i=stat(fname, &sb);
  recs=sb.st_size/26;
  //recs=1000;
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
  //printf("index file %s\n",fname);
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
  long nswath[3];

  fref=37.53472224;

  charerr=strcpy(fname,basename);
  charerr=strcat(fname,".dat");
  i=stat(fname, &sb);
  fp=fopen(fname,"r");
  printf("data file size : %d\n\n",sb.st_size);

  // allocate memory for raw data
  rawdata = (unsigned char *)malloc(sb.st_size*sizeof(unsigned char));
  iptr = (long long *)malloc((recs+1)*sizeof(long long));
  line10 = (long *)malloc((recs+1)*sizeof(long));
  line11 = (long *)malloc((recs+1)*sizeof(long));
  line12 = (long *)malloc((recs+1)*sizeof(long));

  // and read it in
  fread(rawdata,1,sb.st_size,fp);
  printf("Read in %d bytes\n",sb.st_size);
  fclose(fp);

  // scan for line pointers
  nswath[0]=0;nswath[1]=0;nswath[2]=0;
  iptr[0]=0;
  for (i=0;i<recs;i++){
    memcpy(data,&rawdata[iptr[i]],68);
    packetDataLength=int2(&data[4])+1;
    swath=data[64];
    sigtyp=(data[63]&0xf0)/16;
    iptr[i+1]=iptr[i]+packetDataLength+6;
    //if(i%1000==0)printf("%d %d %d\n",i,iptr[i],packetDataLength);
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

  // allocate memory for output swaths
  //  swath10 = (unsigned char *)malloc(nswath[0]*8*30000*sizeof(unsigned char));
  //swath11 = (unsigned char *)malloc(nswath[1]*8*30000*sizeof(unsigned char));
  //swath12 = (unsigned char *)malloc(nswath[2]*8*30000*sizeof(unsigned char));
  outline = (unsigned char *)malloc(8*30000*sizeof(unsigned char));

  // output files opened here
  fpout=fopen("raw.dat","w");
  fpoutq=fopen("rawq.dat","w");
  fpout1=fopen("raw.1.dat","w");
  fpout2=fopen("raw.2.dat","w");
  fpout3=fopen("raw.3.dat","w");

  for (i=0;i<recs;i++){
    if(i%2000==0)printf("decoding line %d\n",i);
    // read full record for line
    memcpy(data,&rawdata[iptr[i]],68);  // retrieve packet length
    packetDataLength=int2(&data[4])+1;
    memcpy(data,&rawdata[iptr[i]],6+packetDataLength); // now load full line
    // decode a bunch of parameters as it's convenient if not very useful
    //    packetId=int2(&data[0]);
    //packetSequenceControl=int2(&data[2]);
    //packetDataLength=int2(&data[4])+1;
    //coarseTime=int4(&data[6]);
    //fineTime=(int2(&data[10])+0.5)*pow(2.,-16.);
    //sync=int4(&data[12]);
    //baqMode=data[37]&31;
    //baqBlockLength=8*(data[38]+1);
    //rangeDecimation=data[40];
    //sampleFreq=sampleFrequency(rangeDecimation);
    //rxGain=data[41]*-0.5;
    //polarity=(int2(&data[42])&32768)/32768;
    //rampRate=pow(-1.,(1.-polarity))*(int2(&data[42])&32767)*fref*fref/pow(2.,21.);
    //polarity=(int2(&data[44])&32768)/32768;
    //startFreq=rampRate/4./fref+pow(-1.,(1.-polarity))*(int2(&data[44])&32767)*fref/pow(2.,14.);
    //pulseLength=int3(&data[46])/fref;
    //rank=data[49]&31;
    //pri=int3(&data[50])/fref;
    //swst=int3(&data[53])/fref;
    //swl=int3(&data[56])/fref;
    //elevationbeam=(data[60]&0xf0)/16;
    sigtyp=(data[63]&0xf0)/16;
    swath=data[64];
    nq=int2(&data[65]);

    // zero pad rest of input array
    for (j=packetDataLength+6;j<65536;j++){
      data[j]=0;
    }

    // sample decoding if echo data
    if(sigtyp==0){
      decode_line(&data[6+62],nq,packetDataLength,&cpxsamps,i);
      time[0]=times[i];

      // assemble output line
      memcpy(outline,data,68);
      //printf("outline %d %d %d %d\n",outline[0],outline[1],outline[2],outline[3]);
      //printf("data %d %d %d %d\n",data[0],data[1],data[2],data[3]);
      memcpy(&outline[68],time,8);
      memcpy(&outline[76],zeros,4);
      memcpy(&outline[80],cpxsamps,8*nq*2);
      memcpy(&outline[80+8*nq*2],zeros,8*(30000-nq*2-80/8));
      //fwrite(outline,8,30000,fpoutq);
	//	fwrite(&data,1,6+62,fpout);
	//fwrite(&time,1,8,fpout);
	//fwrite(&zeros,1,4,fpout);
	//fwrite(&cpxsamps,8,nq*2,fpout);
	//fwrite(&zeros,8,30000-nq*2-80/8,fpout);

	//printf("cpxsamps %f %f %f %f\n",cpxsamps[100],cpxsamps[101],cpxsamps[1000],cpxsamps[1001]);
	//printf("outline start %d %d %d %d %d %d %d %d\n",outline[0],outline[1],outline[2],outline[3],outline[4],outline[5],outline[6],outline[7],outline[8],outline[9]);
	//printf("line counters %d %d %d\n",line10[i],line11[i],line12[i]);
      // sort by swath
      /*
      if(swath==10)memcpy(&swath10[line10[i]*30000*8],outline,30000*8);
      if(swath==11)memcpy(&swath11[line11[i]*30000*8],outline,30000*8);
      if(swath==12)memcpy(&swath12[line12[i]*30000*8],outline,30000*8);
      */
      //      if(swath==10)fwrite(&swath10[line10[i]],8,30000,fpout);
      //if(swath==11)fwrite(&swath11[line10[1]],8,30000,fpout);
      //if(swath==12)fwrite(&swath12[line12[i]],8,30000,fpout);
      // write line to correct swath file
      //      /*
      if(swath==10)fwrite(outline,8,30000,fpout1);
      if(swath==11)fwrite(outline,8,30000,fpout2);
      if(swath==12)fwrite(outline,8,30000,fpout3);
      //*/

    }  //end sigtype loop
  }  //end loop over lines

  /*
        if(swath==10){
	fwrite(&data,1,6+62,fpout1);
	fwrite(&time,1,8,fpout1);
	fwrite(&zeros,1,4,fpout1);
	fwrite(&cpxsamps,8,nq*2,fpout1);
	fwrite(&zeros,8,30000-nq*2-80/8,fpout1);
      }
      if(swath==11){
	fwrite(&data,1,6+62,fpout2);
	fwrite(&time,1,8,fpout2);
	fwrite(&zeros,1,4,fpout2);
	fwrite(&cpxsamps,8,nq*2,fpout2);
	fwrite(&zeros,8,30000-nq*2-80/8,fpout2);
      }
      if(swath==12){
	fwrite(&data,1,6+62,fpout3);
	fwrite(&time,1,8,fpout3);
	fwrite(&zeros,1,4,fpout3);
	fwrite(&cpxsamps,8,nq*2,fpout3);
	fwrite(&zeros,8,30000-nq*2-80/8,fpout3);
      }

    }  // end sigtyp if 
  }  // end line loop
  */
  /*
  printf("Writing swath files out\n");

  bytecount=30000*nswath[0];
  printf("bytecount %d\n",bytecount);
  //for (i=0;i<nswath[0];i++)fwrite(&swath10[8*30000*nswath[0]],8,30000,fpout1);
  fwrite(swath10,8,30000*nswath[0],fpout1);
  bytecount=30000*nswath[1];
  printf("bytecount %d\n",bytecount);
  //  for (i=0;i<nswath[1];i++)fwrite(&swath11[8*30000*nswath[1]],8,30000,fpout2);
  fwrite(swath11,8,30000*nswath[1],fpout2);
  bytecount=30000*nswath[2];
  printf("bytecount %d\n",bytecount);
  //for (i=0;i<nswath[2];i++)fwrite(&swath12[8*30000*nswath[2]],8,30000,fpout3);
  fwrite(swath12,8,30000*nswath[2],fpout3);
  */
  fclose(fpout1);
  fclose(fpout2);
  fclose(fpout3);
}

