//!!!!!!!!!!!!!!
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

struct ellipsoid{
  double a;
  double e2;
};

extern "C" void latlon_ ( ellipsoid *elp, double *xyz, double *llh,int *i_type );
extern "C" void orbitrangetime_(double *xyz,double *timeorbit,double *xx, double *vv,int *numstatevec,double *tmid,double *satx, double *satv,double *tline,double *rngpix);

// pixelint - the routine to compute complex sum in gpu

__global__ void pixelint(cuFloatComplex *burstdata_d, double *satloc_d, double *xyz_d, double *azoff_d, int *demwidth_d, int *aperture_d, int *iaperture_d, int *rawdatalines_d, int *samplesPerBurst_d, double *rngstart_d, double *rngend_d,  double *pixeltime_d, double *dmrg_d, double *wvl_d, cuFloatComplex *outdata_d){

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

/*
// check a few parameters
printf("following for pixelint\n");
printf("parameter check samplesPerBurst %d\n",*samplesPerBurst_d);
printf("parameter check range start end %f %f\n",*rngstart_d,*rngend_d);
printf("parameter check dmrg wvl %f %f\n",*dmrg_d,*wvl_d);
printf("parameter check aperture iaperture %d %d\n",*aperture_d,*iaperture_d);
printf("parameter check satloc %f %f %f\n",satloc_d[0],satloc_d[1],satloc_d[2]);
printf("parameter check xyz %f %f %f\n",xyz_d[0],xyz_d[1],xyz_d[2]);
printf("parameter check azoff_d %f %f %f\n",azoff_d[100],azoff_d[1000],azoff_d[1200]);
printf("parameter check burstdata %f %f %f %f\n",cuCrealf(burstdata_d[0]),cuCimagf(burstdata_d[0]),cuCrealf(burstdata_d[1]),cuCimagf(burstdata_d[1]));
printf("above for pixelint\n");
*/

// outer loop is over pixels in line
for (int pixel = index; pixel < *demwidth_d; pixel+=stride){
    //  loop for complex integration
    cacc = make_cuDoubleComplex(0.,0.);

    if (pixeltime_d[pixel] > 0.){
        azstart=round(azoff_d[pixel] - *aperture_d / 2); 
    	azend=round(azoff_d[pixel] + *aperture_d / 2); 
	azstart=max(azstart,0);
	azend=min(azend,*rawdatalines_d-1);
	//printf("azstart azend %d %d\n",azstart,azend);

	for (azline=azstart; azline<azend; azline++){
	  vectx=xyz_d[3 * pixel + 0]-satloc_d[3*azline];
	  vecty=xyz_d[3 * pixel + 1]-satloc_d[3*azline+1];
	  vectz=xyz_d[3 * pixel + 2]-satloc_d[3*azline+2];
	  range=sqrt(vectx*vectx+vecty*vecty+vectz*vectz);
	  if (range>=*rngstart_d && range <= *rngend_d - *dmrg_d){
	    r=(range - *rngstart_d) / *dmrg_d ;
	    intr=floor(r);
	    fracr=r-intr;

	    cval=cuCaddf( cuCmulf(burstdata_d[azline * *samplesPerBurst_d + intr], make_cuComplex(1-fracr,0.)), cuCmulf( burstdata_d[azline * *samplesPerBurst_d + (intr+1)], make_cuFloatComplex(fracr,0.)));

	    phase = 4. * pi / *wvl_d * range;
	    cphase = make_cuDoubleComplex(cos(phase),sin(phase));
	    cacc = cuCadd(cacc, cuCmul(cuComplexFloatToDouble(cval), cphase));
	  }  // end range test 
	}  // end azline loop
    } // end pixel test
    outdata_d[pixel]=cuComplexDoubleToFloat(cacc);

} // end pixel loop

}

extern "C" void azimuth_compress_(float complex *burstdata,double *satloc,int *rawdatalines,int *samplesPerBurst,int *demwidth,int *demlength,int *fdout,int *fddem,double *deltalat,double *deltalon,double *firstlat,double *firstlon, double *latlons,double *timeorbit,double *xx,double *vv,int *numstatevec,double *rngstart,double *rngend,double *tstart,double *tend,double *tmid,double *xyz_mid,double *vel_mid,double *t,double *dtaz,double *dmrg,double *wvl,int *aperture,int *iaperture, double *angc0, double *angc1, double *prf)
{

// internal variables
  double lat;
  double *lon;
  short *demin;
  double *azoff;
  double *pixeltime;
  double tline;
  double rngpix;
  double *xyz;
  double umag, fd, veff, td, udotv;

  int naperture, computeflag; // naperture is integration midpoint in pixels, computeflag sets whether to call integrator
  int pixel,line,i;
  int nbytes;
  long long int iaddr;
  float complex *outdata, *indata;

// variables for openmp pragma argument passing
  int ompdemwidth=*demwidth;
  double omptmid=*tmid;
  double ompangc0=*angc0, ompangc1=*angc1;
  double ompprf=*prf;
  int omprawdatalines=*rawdatalines;
  double omptstart=*tstart, omptend=*tend;
  int ompaperture=*aperture;
  double ompwvl=*wvl;
   
// malloc cpu arrays
  lon = (double *) malloc(*demwidth * sizeof(double));
  demin = (short *) malloc(*demwidth * sizeof(short));
  azoff = (double *) malloc(*demwidth * sizeof(double));
  pixeltime = (double *) malloc(*demwidth * sizeof(double));
  xyz = (double *) malloc(sizeof(double) * 3 * *demwidth);
  outdata = (float complex *)malloc(*demwidth * sizeof(float complex));
  indata = (float complex *)malloc(*demwidth * sizeof(float complex));

// gpu array definitions
   cuFloatComplex *burstdata_d;
   double *satloc_d;

   cudaMalloc( (void **)&burstdata_d, sizeof(float complex) * *rawdatalines * *samplesPerBurst);
   cudaMalloc( (void **)&satloc_d, sizeof(double) * 3 * *rawdatalines);

// other variables we'll need in the gpu
   double *azoff_d, *xyz_d, *pixeltime_d;
   int    *aperture_d, *iaperture_d, *rawdatalines_d, *samplesPerBurst_d;
   int    *demwidth_d;
   double *rngstart_d, *rngend_d, *dmrg_d, *wvl_d;
   cuFloatComplex *outdata_d;
   double *tstart_d, *tend_d, *tline_d;

   cudaMalloc( (void **)&pixeltime_d, sizeof(double) * *demwidth);
   cudaMalloc( (void **)&azoff_d, sizeof(double) * *demwidth);
   cudaMalloc( (void **)&aperture_d, sizeof(int));
   cudaMalloc( (void **)&iaperture_d, sizeof(int));
   cudaMalloc( (void **)&rawdatalines_d, sizeof(int));
   cudaMalloc( (void **)&xyz_d, sizeof(double) * 3 * *demwidth);
   cudaMalloc( (void **)&samplesPerBurst_d, sizeof(int));
   cudaMalloc( (void **)&demwidth_d, sizeof(int));
   cudaMalloc( (void **)&rngstart_d, sizeof(double));
   cudaMalloc( (void **)&rngend_d, sizeof(double));
   cudaMalloc( (void **)&dmrg_d, sizeof(double));
   cudaMalloc( (void **)&tstart_d, sizeof(double));
   cudaMalloc( (void **)&tend_d, sizeof(double));
   cudaMalloc( (void **)&tline_d, sizeof(double));
   cudaMalloc( (void **)&wvl_d, sizeof(double));
   cudaMalloc( (void **)&outdata_d, sizeof(float complex) * *demwidth);

// constants and such
  int LLH_2_XYZ=1;

  double pi,deg2rad;

  struct ellipsoid elp = { 6378137.0, 0.0066943799901499996};
 
  pi = 4. * atan2(1.0,1.0);
  deg2rad = pi/180.0;

//  get some basic gpu architecture info
    int blockSize = 256;
    int numBlocks = (*demwidth+blockSize-1)/blockSize;
    printf("GPU blocksize, numblocks: %d %d\n",blockSize,numBlocks);

//  start setting up shared data arrays in gpu 
  cudaMemcpy( burstdata_d, burstdata, sizeof(float complex) * *rawdatalines * *samplesPerBurst, cudaMemcpyHostToDevice );
  cudaMemcpy( satloc_d, satloc, sizeof(double) * *rawdatalines * 3, cudaMemcpyHostToDevice );

//  and other constants needed in pixel integration 
  cudaMemcpy( aperture_d, aperture, sizeof(int), cudaMemcpyHostToDevice );
  cudaMemcpy( iaperture_d, iaperture, sizeof(int), cudaMemcpyHostToDevice );
  cudaMemcpy( rawdatalines_d, rawdatalines, sizeof(int), cudaMemcpyHostToDevice );
  cudaMemcpy( samplesPerBurst_d, samplesPerBurst, sizeof(int), cudaMemcpyHostToDevice );
  cudaMemcpy( demwidth_d, demwidth, sizeof(int), cudaMemcpyHostToDevice );
  cudaMemcpy( rngstart_d, rngstart, sizeof(double), cudaMemcpyHostToDevice );
  cudaMemcpy( rngend_d, rngend, sizeof(double), cudaMemcpyHostToDevice );
  cudaMemcpy( dmrg_d, dmrg, sizeof(double), cudaMemcpyHostToDevice );
  cudaMemcpy( wvl_d, wvl, sizeof(double), cudaMemcpyHostToDevice );

//  begin loop over lines in dem 
  for (i=0;i<*demwidth;i++){
    lon[i]=*firstlon + (i-1)* *deltalon;
  }

  for (line=0;line<*demlength;line++){
    for (i=0;i<*demwidth;i++)outdata[i]=0.+0.*I;
    lat=*firstlat + line * *deltalat;

    if (lat>latlons[0]) {
      if (lat<latlons[1]) {
	//Read in this line from DEM
	iaddr=(line-1)* *demwidth * 2;
	nbytes=lseek(*fddem, (off_t) iaddr, SEEK_SET);
	iaddr= *demwidth * 2;
	nbytes=read(*fddem,demin,iaddr);
	if (nbytes < 0) printf("dem read error %d\n",nbytes);

	if (line % 1000 == 1) printf("Processing line: %d\n",line);
	computeflag=0;

	// parallelize the pixel loop
	#pragma omp parallel for private(pixel,tline,rngpix,umag,udotv,fd,veff,td,naperture) shared(ompdemwidth,lat,deg2rad,demin,elp,LLH_2_XYZ,xyz,omptmid,xyz_mid,vel_mid,timeorbit,xx,vv,numstatevec,ompwvl,pi,ompangc0,ompangc1,ompprf,omprawdatalines,pixeltime,azoff,omptstart,omptend,ompaperture,computeflag)
	for (pixel=0; pixel< ompdemwidth; pixel++){
	// local array definitions for omp loop
	  double *llh = (double *) malloc(sizeof(double)*3);
	  double *xyztemp = (double *) malloc(sizeof(double)*3);
	  double *satx = (double *) malloc(sizeof(double)*3);
	  double *satv = (double *) malloc(sizeof(double)*3);
	  double *unitlookvector = (double *) malloc(sizeof(double)*3);

	  llh[0] = lat * deg2rad;
	  llh[1] = lon[pixel] * deg2rad;
	  llh[2] = demin[pixel];
	  latlon_(&elp,xyztemp,llh,&LLH_2_XYZ);
	  xyz[pixel * 3]=xyztemp[0];
	  xyz[pixel * 3+1]=xyztemp[1];
	  xyz[pixel * 3+2]=xyztemp[2];
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
	  
	  pixeltime[pixel]=-999999.;
	  azoff[pixel]=0;
	  if (tline > omptstart-2. && tline < omptend+2.){
	    if (naperture > ompaperture/2 && naperture < omprawdatalines- ompaperture/2){
	      azoff[pixel] = naperture; 
              pixeltime[pixel]=tline;
	      computeflag=1;
	      }
	  }
	  free(llh);
	  free(xyztemp);
	  free(satx);
	  free(satv);
	  free(unitlookvector);
	} // end pixel loop

	if (computeflag == 1){
	// put pixel specific info into gpu
	cudaMemcpy( pixeltime_d, pixeltime, sizeof(double) * *demwidth, cudaMemcpyHostToDevice );
	cudaMemcpy( xyz_d, xyz, sizeof(double) * 3 * *demwidth, cudaMemcpyHostToDevice );
	cudaMemcpy( azoff_d, azoff, sizeof(double) * *demwidth, cudaMemcpyHostToDevice );

	pixelint<<< numBlocks,blockSize >>>(burstdata_d,satloc_d,xyz_d,azoff_d, demwidth_d, aperture_d,iaperture_d,rawdatalines_d,samplesPerBurst_d,rngstart_d,rngend_d,pixeltime_d,dmrg_d,wvl_d,outdata_d);
	cudaDeviceSynchronize();
	cudaMemcpy( outdata, outdata_d, sizeof(float complex) * *demwidth, cudaMemcpyDeviceToHost );

	// get existing data for that line
	iaddr=(line-1)* *demwidth * 8;
	nbytes=lseek(*fdout, (off_t) iaddr, SEEK_SET);
	iaddr= *demwidth * 8;
	nbytes=read(*fdout,indata,iaddr);
	// update if pixel computed
	for (pixel=0; pixel< *demwidth; pixel++){
	    if(abs(creal(outdata[pixel])) > 1.e-18)indata[pixel]=outdata[pixel];
	    }	    
	// write line to file
	iaddr=(line-1)* *demwidth * 8;
	nbytes=lseek(*fdout, (off_t) iaddr, SEEK_SET);
	iaddr= *demwidth * 8;
	nbytes=write(*fdout,indata,iaddr);
	}  // end compute flag test

      } // end if lat in bounds
      
    } // end if lat in bounds #2

  } // end line loop

  // free up memory in gpu
  cudaFree(burstdata_d);
  cudaFree(satloc_d);
  cudaFree(azoff_d);
  cudaFree(pixeltime_d);
  cudaFree(iaperture_d);
  cudaFree(rawdatalines_d);
  cudaFree(xyz_d);
  cudaFree(samplesPerBurst_d);
  cudaFree(rngstart_d);
  cudaFree(rngend_d);
  cudaFree(dmrg_d);
  cudaFree(wvl_d);

}

