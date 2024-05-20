//!!!!!!!!!!!!!!!
//!
//!
//!  azimuth compression subroutine for use in Sentinel back projection processor
//!    called from a fortran main program
//!
//!
//!!!!!!!!!!!!!!

#include <stdlib.h>
#include <stdio.h>
#include <complex.h>
#include <math.h>
#include <unistd.h>
#include <cuComplex.h>
#include <cuda.h>
#include <cmath>
#include <omp.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>

struct ellipsoid{
  double a;
  double e2;
};

extern "C" void latlon_ ( ellipsoid *elp, double *xyz, double *llh,int *i_type );
extern "C" void orbitrangetime_(double *xyz,double *timeorbit,double *xx, double *vv,int *numstatevec,double *tmid,double *satx, double *satv,double *tline,double *rngpix);

// pixelint - the routine to compute complex sum in gpu

__global__ void pixelint(cuFloatComplex *burstdata_d, double *satloc_d, double *xyz_d, double *azoff_d, int demwidth_d, int aperture_d, int iaperture_d, int rawdatalines_d, int samplesPerBurst_d, double rngstart_d, double rngend_d,  double dmrg_d, double wvl_d, cuFloatComplex *outdata_d, int nlines_d){

  // set up gpu grid parameters
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;
  //printf("index stride %d %d\n",index,stride);

  // internal variables for integration
  cuDoubleComplex cacc, cphase;
  cuFloatComplex cval;
  int intr;
  double range, fracr,phase,r,vectx,vecty,vectz,pi;
  int azline;
  int azstart, azend;

  pi=4.*atan2(1.,1.);

  // loop over 1D version of 2D arrays
  for (long loop = index; loop < nlines_d * demwidth_d; loop +=stride){

    cacc = make_cuDoubleComplex(0.,0.);
    if (azoff_d[loop] > 0.){
      azstart=round(azoff_d[loop] - aperture_d / 2);
      azend=round(azoff_d[loop] + aperture_d / 2);
      azstart=max(azstart,0);
      azend=min(azend,rawdatalines_d-1);
      for (azline=azstart; azline<azend; azline++){
	vectx=xyz_d[loop * 3 + 0]-satloc_d[3*azline];
	vecty=xyz_d[loop * 3 + 1]-satloc_d[3*azline+1];
	vectz=xyz_d[loop * 3 + 2]-satloc_d[3*azline+2];
	range=sqrt(vectx*vectx+vecty*vecty+vectz*vectz);
	if (range>=rngstart_d && range <= rngend_d - dmrg_d){
	  r=(range - rngstart_d) / dmrg_d ;
	  intr=floor(r);
	  fracr=r-intr;

	  cval=cuCaddf( cuCmulf(burstdata_d[azline * samplesPerBurst_d + intr], make_cuComplex(1-fracr,0.)), cuCmulf( burstdata_d[azline * samplesPerBurst_d + (intr+1)], make_cuFloatComplex(fracr,0.)));

	  phase = 4. * pi / wvl_d * range;
	  cphase = make_cuDoubleComplex(cos(phase),sin(phase));
	  cacc = cuCadd(cacc, cuCmul(cuComplexFloatToDouble(cval), cphase));
	}  // end range test
      }  // end azline loop
    } // end pixel test

    outdata_d[loop]=cuComplexDoubleToFloat(cacc);
    outdata_d[loop]=cuCmulf(outdata_d[loop],make_cuFloatComplex(1./(azend-azstart),0.)); // part of sigma calibration for az integratiom length
  } // end loop loop

}

//  set xyz array
__global__ void setxyz(short *demin_d, double *xyz_d, double *xyzfit_d, double *azoff_d, int demwidth_d, double firstlat_d, double deltalat_d, double firstlon_d, double deltalon_d, int firstline_d, int nlines_d, int firstpix_d, int lastpix_d){

  // local array definitions for loop
  double *llh = (double *) malloc(sizeof(double)*3);
  double *xyztemp = (double *) malloc(sizeof(double)*3);
  double *satx = (double *) malloc(sizeof(double)*3);
  double *satv = (double *) malloc(sizeof(double)*3);
  double *unitlookvector = (double *) malloc(sizeof(double)*3);
  //double * lon;
  //cudaMalloc (&lon, sizeof(double) * demwidth_d);

  //  struct ellipsoid elp = { 6378137.0, 0.0066943799901499996};

  // set up gpu grid parameters
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;

  double pi = 4.0*atan2(1.,1.);
  double deg2rad = pi / 180.;
  double a = 6378137.0;
  double e2 = 0.0066943799901499996;
  double re;
  int line, pixel;
  double lat;
  long loop;
  long long xyzoffset;
  double llhlat, llhlon, llhhgt;

  // loop over full array
  for (loop = index; loop < demwidth_d * nlines_d; loop+=stride){
    line = int (loop / demwidth_d);
    pixel = loop - line * demwidth_d;
    lat = firstlat_d + (line + firstline_d) * deltalat_d;

    if (pixel >= firstpix_d && pixel <= lastpix_d){

      xyzoffset = ((long long int) line * (long long int) demwidth_d + (long long int) pixel ) * (long long int) 3;

      llhlat = lat * deg2rad;
      llhlon = (firstlon_d + pixel * deltalon_d) * deg2rad;
      llhhgt = demin_d[xyzoffset / 3];

      re = a/sqrt(1.0 - e2*sin(llhlat)*sin(llhlat));
          
      xyz_d[xyzoffset+0] = (re + llhhgt)*cos(llhlat)*cos(llhlon);
      xyz_d[xyzoffset+1] = (re + llhhgt)*cos(llhlat)*sin(llhlon);
      xyz_d[xyzoffset+2] = (re - (re * e2) + llhhgt)*sin(llhlat);        

      if(pixel == firstpix_d){
        xyzfit_d[line * 9 + 0]=xyz_d[xyzoffset+0];
        xyzfit_d[line * 9 + 1]=xyz_d[xyzoffset+1];
        xyzfit_d[line * 9 + 2]=xyz_d[xyzoffset+2];
        }
      if(pixel == firstpix_d + int((lastpix_d-firstpix_d)/2-1)){
        xyzfit_d[line * 9 + 3]=xyz_d[xyzoffset+0];
        xyzfit_d[line * 9 + 4]=xyz_d[xyzoffset+1];
        xyzfit_d[line * 9 + 5]=xyz_d[xyzoffset+2];
        }
      if(pixel == firstpix_d + 2 * int((lastpix_d-firstpix_d)/2-1)){
        xyzfit_d[line * 9 + 6]=xyz_d[xyzoffset+0];
        xyzfit_d[line * 9 + 7]=xyz_d[xyzoffset+1];
        xyzfit_d[line * 9 + 8]=xyz_d[xyzoffset+2];
	}

    }  // end pixel test
  }  // end loop loop
}  // end routine setxyz

//  set azoff array
__global__ void setazoff(double *coef_d, double *azoff_d, int demwidth_d, int nlines_d, int firstpix_d, int lastpix_d, int aperture_d, int rawdatalines_d){

  // set up gpu grid parameters
  int index = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = blockDim.x * gridDim.x;

  int line, pixel;
  double fit, arg;
  long loop;

  for (loop = index; loop < demwidth_d * nlines_d; loop+=stride){
    line = int (loop / demwidth_d);
    pixel = loop - line * demwidth_d;
//    printf("line pixel %d %d\n",line, pixel);

    if (pixel >= firstpix_d && pixel <= lastpix_d){
      arg=(float(pixel-firstpix_d)/float(lastpix_d-firstpix_d)*2.-1.);
      fit=coef_d[line*3+0]*arg*arg+coef_d[line*3+1]*arg+coef_d[line*3+2];
      azoff_d[line * demwidth_d + pixel]=-1;
      if (fit > aperture_d/2 && fit < rawdatalines_d- aperture_d/2){
        azoff_d[line * demwidth_d + pixel]=fit;
      }  // end fit in aperture test
  }  // end pixel test
  }  // end loop loop
}  //end setazoff routine


extern "C" void azimuth_compress_(
				  _Complex float *burstdata,
				  double *satloc,
				  int *rawdatalines,
				  int *samplesPerBurst,
				  int *demwidth,
				  int *demlength,
				  int *fdout,
				  int *fddem,
				  double *deltalat,
				  double *deltalon,
				  double *firstlat,
				  double *firstlon,
				  double *latlons,
				  double *timeorbit,
				  double *xx,
				  double *vv,
				  int *numstatevec,
				  double *rngstart,
				  double *rngend,
				  double *tstart,
				  double *tend,
				  double *tmid,
				  double *xyz_mid,
				  double *vel_mid,
				  double *t,
				  double *dtaz,
				  double *dmrg,
				  double *wvl,
				  int *aperture,
				  int *iaperture,
				  double *angc0,
				  double *angc1,
				  double *prf)
{

  // internal variables
  double *lon;
  short *demin;
//  double *azoff;
  double tline;
  double rngpix;
  //double *xyz;
  double *xyzfit;
  double *coef;
  double umag, fd, veff, td, udotv;
  int firstline, lastline;  // limits on line loop
  int firstpix, lastpix;  // limits on pixel loop
  long long int arraysize;
  FILE *fpout; // stream for file descriptor *fdout

  int naperture; // naperture is integration midpoint in pixels
  int y1,y2,y3;
  int pixel,line,i;
  long long int nbytes;
  int nlines;
  off_t iaddr_off_t;
  size_t iaddr_size_t;
  _Complex float *outdata, *indata;

  // variables for openmp pragma argument passing
//  int ompdemwidth=*demwidth;
  double omptmid=*tmid;
  double ompangc0=*angc0, ompangc1=*angc1;
  double ompprf=*prf;
  int omprawdatalines=*rawdatalines;
//  int ompaperture=*aperture;
  double ompwvl=*wvl;

//  struct timeval t0;
//  double time0,time1;

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // set a gpudevice

  long int getgpu = cudaSetDevice(0);
//  printf("GPU set return: %d\n",getgpu);
  if (getgpu != 0){
     printf("Can't grab GPU %ld\n",getgpu);
     FILE *fgetgpu = NULL;
     fgetgpu =fopen("getgpulog","a");
     fprintf(fgetgpu,"Can't grab GPU %ld\n",getgpu);
     fclose(fgetgpu);
     }    


/*
// list some gpu attributes to see where we connected
  int nDevices;

  cudaGetDeviceCount(&nDevices);
  for (int i = 0; i < nDevices; i++) {
    cudaDeviceProp prop;
    cudaGetDeviceProperties(&prop, i);
    printf("Device Number: %d\n", i);	
    printf("  Device name: %s\n", prop.name);
}
*/

//  int *gpudevice;
//  long int getdevice = cudaGetDevice(*gpudevice);
//  printf("Using gpu %d\n",gpudevice);
 
//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" grab a gpu time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // how much memory will we need to allocate?
//  printf("latlons %f %f %f %f\n",latlons[0],latlons[1],latlons[2],latlons[3]);
  lastline=int((latlons[0] - *firstlat) / *deltalat );
  firstline=int((latlons[1] - *firstlat) / *deltalat );
  if (firstline < 0) firstline=0;
  if (lastline < 0) lastline=0;
  if (firstline > *demlength-1) firstline= *demlength -1;
  if (lastline > *demlength-1) lastline= *demlength -1;
  nlines=lastline-firstline+1;
  printf("Burst line limits, size (first,last,nlines) %d %d %d\n",firstline,lastline,nlines);

  // malloc cpu arrays
  arraysize = (long int) nlines * (long int) *demwidth;
  // printf("arraysize nlines demwidth: %ld %d %d\n",arraysize, nlines, *demwidth);
lon = (double *) malloc(*demwidth * sizeof(double));
  demin = (short *) malloc(arraysize * sizeof(short));
//  azoff = (double *) malloc(arraysize * sizeof(double));
  //  pixeltime = (double *) malloc(*demwidth * sizeof(double));
  // xyz = (double *) malloc(arraysize * sizeof(double) * 3);
  xyzfit = (double *) malloc(nlines * sizeof(double) * 9);
  coef = (double *) malloc(nlines * sizeof(double) * 3);
  outdata = (_Complex float *)malloc(arraysize * sizeof(_Complex float));
  indata = (_Complex float *)malloc(arraysize * sizeof(_Complex float));
  // printf("bytes for indata outdata %lld\n",arraysize*8);
  
//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" cpu malloc time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // gpu array definitions
  cuFloatComplex *burstdata_d;
  double *satloc_d;
  double *azoff_d, *xyz_d;
  double *xyzfit_d;
  double *coef_d;
  cuFloatComplex *outdata_d;
  short *demin_d;

  cudaMalloc( (void **)&burstdata_d, sizeof(_Complex float) * *rawdatalines * *samplesPerBurst);
  cudaMalloc( (void **)&satloc_d, sizeof(double) * 3 * *rawdatalines);
  cudaMalloc( (void **)&azoff_d, sizeof(double) * arraysize);
  cudaMalloc( (void **)&xyz_d, sizeof(double) * 3 * arraysize);
  cudaMalloc( (void **)&xyzfit_d, sizeof(double) * 9 * nlines);
  cudaMalloc( (void **)&coef_d, sizeof(double) * 3 * nlines);
  cudaMalloc( (void **)&outdata_d, sizeof(_Complex float) * arraysize);
  cudaMalloc( (void **)&demin_d, sizeof(short) * arraysize);

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" gpu malloc time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // constants and such
  double pi;
  pi = 4. * atan2(1.0,1.0);

  //  get some basic gpu architecture info
  int blockSize = 256;
  int numBlocks = (*demwidth+blockSize-1)/blockSize;
//  printf("GPU blocksize, numblocks: %d %d\n",blockSize,numBlocks);

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" set constants time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  //  start setting up shared data arrays in gpu, transfer raw data and satellite locations
  cudaMemcpy( burstdata_d, burstdata, sizeof(_Complex float) * *rawdatalines * *samplesPerBurst, cudaMemcpyHostToDevice );
  cudaMemcpy( satloc_d, satloc, sizeof(double) * *rawdatalines * 3, cudaMemcpyHostToDevice );

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" transfer burst and satloc to gpu time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // set up longitude loop
  firstpix=(latlons[2]-*firstlon)/ *deltalon; if (firstpix < 0)firstpix=0;
  lastpix=(latlons[3]-*firstlon)/ *deltalon; if (lastpix > *demwidth)lastpix= *demwidth;

  //  define longitude array
  for (i=0;i<*demwidth;i++){
    lon[i]=*firstlon + (i-1)* *deltalon;
  }

  // zero out data array before integration
  for (int j=0; j<nlines; j++){
    for (i=0;i<*demwidth;i++){
      outdata[i+j * *demwidth]=0.+0.*i;
    }}

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" initialize lon and outdata array time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  //  process full burst, begin by grabbing proper section of DEM
  iaddr_off_t=(long long int) firstline * (long long int) *demwidth * (long long int) 2;
  nbytes=lseek(*fddem, iaddr_off_t, SEEK_SET);
  iaddr_size_t= (long long int) nlines * (long long int) *demwidth * (long long int) 2;
  nbytes=read(*fddem,demin,iaddr_size_t);
  if (nbytes < 0) {
    printf("dem read error %lld\n",nbytes);
    printf("iaddr_off_t %ld, iaddr_size_t %ld\n",iaddr_off_t, iaddr_size_t);
    printf("nlines %d, demwidth %d\n",nlines,*demwidth);
  }
         
//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" DEM section read in time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  //  set xyz array in gpu
  cudaMemcpy( demin_d, demin, sizeof(short) * arraysize, cudaMemcpyHostToDevice );
  setxyz<<< numBlocks,blockSize >>>(demin_d, xyz_d, xyzfit_d, azoff_d, *demwidth, *firstlat, *deltalat, *firstlon, *deltalon, firstline, nlines, firstpix, lastpix);
  cudaDeviceSynchronize();
  cudaMemcpy( xyzfit, xyzfit_d, sizeof(double) * nlines * 9, cudaMemcpyDeviceToHost );

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" compute xyz in gpu time %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  // remainder of azoff calcs before integration

  double *xyztemp = (double *) malloc(sizeof(double)*3);
  double *satx = (double *) malloc(sizeof(double)*3);
  double *satv = (double *) malloc(sizeof(double)*3);
  double *unitlookvector = (double *) malloc(sizeof(double)*3);

  //  loop over lines in DEM for burst
  for (line=firstline; line<lastline;line++){

    for (int ipix=0; ipix<3; ipix++){
      pixel = firstpix + ipix * int((lastpix - firstpix)/2 - 1);

      //xyzoffset = ((long long int) (line-firstline) * (long long int) ompdemwidth + (long long int) pixel ) * (long long int) 3;

      // for start, middle, and end get integration midpoints

      xyztemp[0] = xyzfit[(line-firstline)*9 + ipix * 3 + 0];
      xyztemp[1] = xyzfit[(line-firstline)*9 + ipix * 3 + 1];
      xyztemp[2] = xyzfit[(line-firstline)*9 + ipix * 3 + 2];

      tline = omptmid;
      satx[0] = xyz_mid[0];satx[1] = xyz_mid[1];satx[2] = xyz_mid[2];
      satv[0] = vel_mid[0];satv[1] = vel_mid[1];satv[2] = vel_mid[2];
      // get the zero doppler location of the satellite
      orbitrangetime_(xyztemp,timeorbit,xx,vv,numstatevec,&omptmid,satx,satv,&tline,&rngpix);

      // tops-specific geometry calculations
      unitlookvector[0]=(xyztemp[0]-satx[0]); unitlookvector[1]=(xyztemp[1]-satx[1]); unitlookvector[2]=(xyztemp[2]-satx[2]);
      umag=sqrt(unitlookvector[0]*unitlookvector[0]+unitlookvector[1]*unitlookvector[1]+unitlookvector[2]*unitlookvector[2]);
      unitlookvector[0]=unitlookvector[0]/umag; unitlookvector[1]=unitlookvector[1]/umag; unitlookvector[2]=unitlookvector[2]/umag;
      udotv=satv[0]*unitlookvector[0]+satv[1]*unitlookvector[1]+satv[2]*unitlookvector[2];
      fd=(2.0/ ompwvl) * udotv;
      veff=sqrt(satv[0]*satv[0]+satv[1]*satv[1]+satv[2]*satv[2]);
      td=(rngpix* (ompwvl)/2./veff/veff*fd-rngpix*(ompangc0)*pi/180./veff)/(1.+rngpix*(ompangc1)*pi/180./veff);
      naperture=td*(ompprf) + omprawdatalines/2;

      if (ipix == 0)y1=naperture;
      if (ipix == 1)y2=naperture;
      if (ipix == 2)y3=naperture;
      // if at last pixel, save coefficients for azoff fit
      if (ipix==2){
         coef[(line-firstline)*3+0]=0.5*(y1+y3-2.*y2);
         coef[(line-firstline)*3+1]=0.5*(y3-y1);
         coef[(line-firstline)*3+2]=y2;
         //a=0.5*(y1+y3-2.*y2);
         //b=0.5*(y3-y1);
         //c=y2;
         }

    } // end pixel loop
  }  // end line loop

// compute azoff array in gpu, parabolas for each line

   cudaMemcpy( coef_d, coef, sizeof(double) * nlines * 3, cudaMemcpyHostToDevice );
   setazoff<<< numBlocks,blockSize >>>(coef_d, azoff_d, *demwidth, nlines, firstpix, lastpix, *aperture, *rawdatalines);

      free(xyztemp);
      free(satx);
      free(satv);
      free(unitlookvector);

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" time in cpu azoff loop %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

    pixelint<<< numBlocks,blockSize >>>(burstdata_d,satloc_d,xyz_d,azoff_d, *demwidth, *aperture, *iaperture, *rawdatalines, *samplesPerBurst, *rngstart, *rngend, *dmrg, *wvl, outdata_d, nlines);

  cudaDeviceSynchronize();
//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" time in gpu pixel integration  %9.3f\n",time1-time0);

//  gettimeofday(&t0, NULL);
//  time0=t0.tv_sec+t0.tv_usec/1.e6;

  cudaMemcpy( outdata, outdata_d, sizeof(_Complex float) * arraysize, cudaMemcpyDeviceToHost );

  // get existing data for that burst
  iaddr_off_t=(long long int) firstline * (long long int) *demwidth * (long long int) 8;
  iaddr_size_t= arraysize * 8;

  //printf("file descriptor: *fdout fdout %lld %lld\n",*fdout,fdout);
  // stream for *fdout file descriptor
  fpout = fdopen(*fdout,"r+");
  // nbytes=lseek(*fdout, iaddr_off_t, SEEK_SET);
  nbytes=fseek(fpout, iaddr_off_t, SEEK_SET);
  //printf("seek nbytes iaddr_off_t %lld %lld\n",nbytes,iaddr_off_t);
  // nbytes=read(*fdout,indata,iaddr_size_t);
  nbytes=fread(indata, 1, iaddr_size_t, fpout);
  //printf("iaddr_off_t iaddr_size_t nbytes arraysize %lld %lld %lld %lld\n",iaddr_off_t,iaddr_size_t,nbytes,arraysize);
//  printf(" pointer %lld\n",line * *demwidth + *demwidth);
  // update if pixel computed
  for (line=0; line < nlines; line++){
    for (pixel=0; pixel< *demwidth; pixel++){
      if(abs(creal(outdata[line * *demwidth +pixel])) > 1.e-18)indata[line * *demwidth + pixel]=outdata[line * *demwidth + pixel];
    }
  }
  // write line to file
  //nbytes=lseek(*fdout, iaddr_off_t, SEEK_SET);
  nbytes=fseek(fpout, iaddr_off_t, SEEK_SET);
  // printf("seek 2 nbytes %lld\n",nbytes);
  //nbytes=write(*fdout,indata,iaddr_size_t);
  nbytes=fwrite(indata, 1, iaddr_size_t, fpout);
  //  printf("iaddr_off_t iaddr_size_t nbytes arraysize %lld %lld %lld %lld\n",iaddr_off_t,iaddr_size_t,nbytes,arraysize);

//  gettimeofday(&t0, NULL);
//  time1=t0.tv_sec+t0.tv_usec/1.e6;
//  printf(" update and output write time %9.3f\n",time1-time0);

  // free up memory in gpu
  cudaFree(burstdata_d);
  cudaFree(satloc_d);
  cudaFree(azoff_d);
  cudaFree(xyz_d);
  cudaFree(xyzfit_d);
  cudaFree(coef_d);
  cudaFree(outdata_d);
  cudaFree(demin_d);

  // free up cpu memory
  free(indata);
  free(outdata);
  free(demin);
  free(xyzfit);
  free(coef);
  free(lon);
  
}

