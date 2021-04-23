//!c  process sentinel swath raw file in range, split bursts
//   c version
//   write from single large buffer to trade memory against file locking

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <fftw3.h>
#include <omp.h>
#include <math.h>

int int2(unsigned char *data){
  return data[1]+256*data[0];
}

unsigned int int3(unsigned char *data){
  return data[2]+data[1]*256+data[0]*256*256;
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

void main(int argc, char *argv[]){
  fftwf_complex *in, *data, *dataspec, *ref, *reftime;
  fftwf_plan planf, plani;
  char *rawfile;
  unsigned char *raw, *out;
  char outname[300];
  int i,j,ipolarity,npts,nsamps,nvalid,nthreads,ipri;
  int ranfft, rangedecimation, pricount, burst, line, maxmaxlines = 0, maxlines[11] = {0};
  int *burstno, *burstline, *lineinburst;
  long long int nlines, currentlineptr, outlineptr;
  double pulselength,rampRate,samplefreq,startfreq;
  double fref, samplefrequency, t, phase, pi, realpart, imagpart;
  FILE *fpout[11];
  FILE *fp;
  struct stat sb;
  omp_lock_t writelock;
  
  if(argc<2){
    printf("usage: %s process_realaperture rawdatafile\n",argv[0]);
    exit(1);
  }

  omp_init_lock(&writelock);  // set up file locking to prevent thread conflict

  //  input file open and size
  rawfile = argv[1];
  fp = fopen(rawfile,"r");
  if (stat(argv[1], &sb) == -1) {
    perror("stat");
    exit(EXIT_FAILURE);
  }  
  nlines = (long long int)sb.st_size/30000/8;
  printf("file %s nlines %lld\n",argv[1],nlines);
  raw = (unsigned char *)malloc(nlines*30000*8);
  fread(raw,nlines*30000*8,sizeof(unsigned char),fp);
  fclose(fp);

  in = (fftwf_complex *)malloc(30000*2*sizeof(float));
  data = (fftwf_complex *)malloc(32768*2*sizeof(float));
  dataspec = (fftwf_complex *)malloc(32768*2*sizeof(float));
  ref = (fftwf_complex *)malloc(32768*2*sizeof(float));
  reftime = (fftwf_complex *)malloc(32768*2*sizeof(float));
  burstno = (int *)malloc(nlines*sizeof(int));
  burstline = (int *)malloc(nlines*sizeof(int));
  lineinburst = (int *)malloc(nlines*sizeof(int));

  //  some global defs
  fref=37.53472224;
  pi=4.*atan2(1.,1.);
  //  set up transforms using fftw3
  ranfft=32768;
  planf = fftwf_plan_dft_1d(ranfft, data, dataspec, FFTW_FORWARD, FFTW_ESTIMATE);
  plani = fftwf_plan_dft_1d(ranfft, dataspec, data, FFTW_BACKWARD, FFTW_ESTIMATE);

  //  decode some needed parameters 
  rangedecimation=raw[40]&255;
  samplefreq=sampleFrequency(rangedecimation);
  ipolarity=(int2(&raw[42])&32768)/32768;
  rampRate=pow(-1.,(1-ipolarity))*(int2(&raw[42])&32767)*fref*fref/pow(2.,21.);
  ipolarity=(int2(&raw[44])&32768)/32768;
  startfreq=rampRate/4./fref+pow(-1.,(1-ipolarity))*(int2(&raw[44])&32767)*fref/pow(2.,14.);
  pulselength=int3(&raw[46])/fref;
  nsamps=int2(&raw[65])*2;
  npts=pulselength*samplefreq;
  nvalid=nsamps-npts;
  //  printf("rangedecimation,samplefreq,ipolarity,rampRate,startfreq,pulselength,nsamps,npts,nvalid\n");
  //  printf("%d %f %d %f %f %f %d %d %d\n",rangedecimation,samplefreq,ipolarity,rampRate,startfreq,pulselength,nsamps,npts,nvalid);

  //  get the sorting instructions
  ipri=0;
  burst=0;
  for(i=0; i<nlines; i++){
    burstline[i]=0;
    burstno[i]=0;
    lineinburst[i]=0;
  }

  for(i=0; i<nlines; i++){
    if(i%2000 == 0)printf("Sorting line %d\n",i);
    pricount=int4(&raw[(long long int)i*(long long int)240000+33]);
    if(pricount-ipri > 1000)burst=burst+1;
    burstline[burst]=burstline[burst]+1;
    burstno[i]=burst;
    lineinburst[i]=burstline[burst];
    ipri=pricount;
    if(burst < 12){
      if(lineinburst[i] > maxlines[burst-1])maxlines[burst-1]=lineinburst[i];
      if(maxlines[burst-1] > maxmaxlines)maxmaxlines=maxlines[burst-1];
    }
    //    if(i%2000 == 0)printf("pricount burstline[burst] burstno[i] lineinburst[i] %d %d %d %d\n",pricount,burstline[burst],burstno[i],lineinburst[i]);
  }
  printf("Sorted %lld lines of raw data.\n",nlines);
  //printf("%d %d %d %d %d %d %d %d %d %d %d\n",maxlines[0],maxlines[1],maxlines[2],maxlines[3],maxlines[4],maxlines[5],maxlines[6],maxlines[7],maxlines[8],maxlines[9],maxlines[10]);
  printf("Largest burst %d lines.\n",maxmaxlines);

  //  for (i=0; i<10; i++){
  //    printf("i burstno lineinburst %d %d %d %d\n",i,burstno[i],lineinburst[i],ipri);
  //  }
 
  //  open the output files
  for(i=0; i<11;i++){
    sprintf(outname,"burst%d",i+1);
    //    printf("opening file %s\n",outname);
    fpout[i]=fopen(outname,"w");
  }

  //  create a reference function
  for(i=0; i<32768;i++){data[i][0]=0.;data[i][1]=0.;}
  for(j=0; j<npts; j++){
    t=j/samplefreq;
    phase=2.*pi*startfreq*t+pi*rampRate*t*t;
    data[j][0]=cos(phase)/npts;
    data[j][1]=sin(phase)/npts;
  }
  fftwf_execute(planf);
  for(j=0; j<32768; j++){
    ref[j][0]=dataspec[j][0];
    ref[j][1]=dataspec[j][1];
  }

  //  FILE *fpref = fopen("ref.dat","w");
  //  for(i=0; i<32768; i++){
  //    fprintf(fpref,"%f\n",sqrt(ref[i][0]*ref[i][0]+ref[i][1]*ref[i][1]));
  //  }
  //  fclose(fpref);

  //how many threads?
  //nthreads = omp_get_max_threads();
  //printf("Using %d threads\n",nthreads);

  //  create an output buffer to bypass file locks for writes
  out = (unsigned char *)malloc((long int)11 * (long int)(nvalid+10) * maxmaxlines * 8 * sizeof(unsigned char));
  
  //  loop over each line
#pragma omp parallel for shared(nlines,raw,out,nsamps,planf,plani,ref,burstno,lineinburst,nvalid,fpout) private(currentlineptr,j,realpart,imagpart,outlineptr)
  for (i=0; i<nlines; i++){
    // local arrays for omp loop
    fftwf_complex *loopdata = (fftwf_complex *)malloc(32768*2*sizeof(float));
    fftwf_complex *loopdataspec = (fftwf_complex *)malloc(32768*2*sizeof(float));
    unsigned char *header = (unsigned char *)malloc(80*sizeof(unsigned char));

    if(i%2000 == 0)printf("Processing line %d\n",i);
    // move line of raw data into header and data array
    currentlineptr = (long long int)i * (long long int)30000 * 8;
    memcpy(header,&raw[currentlineptr],80);
    memcpy(loopdata,&raw[currentlineptr+80],nsamps*8);
    for (j=nsamps; j<32768; j++){loopdata[j][0]=0.;loopdata[j][1]=0.;}  // zero pad input array
    fftwf_execute_dft(planf,loopdata,loopdataspec);  // transform in range
    for (j=0; j<32768; j++){
      realpart=loopdataspec[j][0]*ref[j][0]+loopdataspec[j][1]*ref[j][1];
      imagpart=-loopdataspec[j][0]*ref[j][1]+loopdataspec[j][1]*ref[j][0];
      loopdataspec[j][0]=realpart;
      loopdataspec[j][1]=imagpart;
    }
  
    fftwf_execute_dft(plani,loopdataspec,loopdata);  // inverse transform

    //    printf("burstno[i] lineinburst[i] %d %d\n",burstno[i],lineinburst[i]);
    if((burstno[i] > 0) && (burstno[i] < 12)){
      outlineptr = (long long int)(burstno[i]-1) * maxmaxlines * (nvalid+10) * 8 + (long long int)(lineinburst[i]-1) * (nvalid+10) * 8;
      memcpy(&out[outlineptr],header,80);
      memcpy(&out[outlineptr+80],loopdata,(long int)nvalid*(long int)8);
      /*
      omp_set_lock(&writelock);
      fseek(fpout[burstno[i]-1],(lineinburst[i]-1)*(nvalid+10)*8,SEEK_SET);
      fwrite(header, 80, sizeof(unsigned char),fpout[burstno[i]-1]);
      fwrite(loopdata, (long int)nvalid*(long int)8, sizeof(unsigned char),fpout[burstno[i]-1]);
      omp_unset_lock(&writelock);
      */
    }

    free(loopdata);
    free(loopdataspec);
    free(header);

  }  // end parallel loop over line i (nlines)

  
  printf("Processed %lld lines of length %d plus header of 10\n",nlines,nvalid);

  //  do the file writes
  for(i=0;i<11;i++){
    outlineptr = (long long int) i * (long long int) maxmaxlines * (long long int) (nvalid+10) * 8;
    fwrite(&out[outlineptr], (long long int)maxlines[i] * (long int)(nvalid+10)*(long int)8, sizeof(unsigned char), fpout[i]);
    //    printf(" %d %d %lld\n",maxlines[i], nvalid, (long long int)maxlines[i] * (long int)(nvalid+10) * (long int)8);
  }
  free(out);
  
  FILE *fpsamples = fopen("rangesamples","w");
  fprintf(fpsamples,"%d\n",nvalid+10);
  fclose(fpsamples);

  //  omp_destroy_lock(&writelock); //  release filelock for neater cleanup
}

