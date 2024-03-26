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
#include <omp.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>

/**
 * Find maximum between two numbers.
 */
int max(int num1, int num2)
{
    return (num1 > num2 ) ? num1 : num2;
}

/**
 * Find minimum between two numbers.
 */
int min(int num1, int num2) 
{
    return (num1 > num2 ) ? num2 : num1;
}

extern void orbitrangetime_(double *xyz,double *timeorbit,double *xx, double *vv,int *numstatevec,double *tmid,double *satx, double *satv,double *tline,double *rngpix);

// pixelint - the routine to compute complex sum in cpu

void pixelint(_Complex float *burstdata_d, double *satloc_d, double *xyz_d, double *azoff_d, int demwidth_d, int aperture_d, int iaperture_d, int rawdatalines_d, int samplesPerBurst_d, double rngstart_d, double rngend_d,  double dmrg_d, double wvl_d, _Complex float *outdata_d, int nlines_d){

  // internal variables for integration
  _Complex double cacc, cphase;
  _Complex float cval;
  int intr;
  double range, fracr,phase,r,vectx,vecty,vectz,pi;
  int azline;
  int azstart, azend;
  long loop;

  pi=4.*atan2(1.,1.);

  // loop over 1D version of 2D arrays
#pragma omp parallel for shared(nlines_d, demwidth_d, azoff_d, aperture_d, rawdatalines_d, xyz_d, satloc_d, rngstart_d, rngend_d, dmrg_d, burstdata_d, samplesPerBurst_d, pi, wvl_d, outdata_d) private(cacc, azstart, azend, azline, vectx, vecty, vectz, range, r, intr, fracr, cval, phase, cphase)
  for (loop = 0; loop < nlines_d * demwidth_d; loop++){

    cacc = 0. + 0. * I;
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

	  cval=burstdata_d[azline * samplesPerBurst_d + intr]*(1-fracr)+burstdata_d[azline * samplesPerBurst_d + (intr+1)]*fracr;

	  phase = 4. * pi / wvl_d * range;
	  cphase = cos(phase) + sin(phase) * I;
	  cacc = cacc+cval*cphase;
	}  // end range test
      }  // end azline loop
    } // end pixel test

    outdata_d[loop]=cacc;

  } // end loop loop and parallel section

}

//  set xyz array
void setxyz(short *demin_d, double *xyz_d, double *xyzfit_d, double *azoff_d, int demwidth_d, double firstlat_d, double deltalat_d, double firstlon_d, double deltalon_d, int firstline_d, int nlines_d, int firstpix_d, int lastpix_d){

  // local array definitions for loop
  double *llh = (double *) malloc(sizeof(double)*3);
  double *xyztemp = (double *) malloc(sizeof(double)*3);
  double *satx = (double *) malloc(sizeof(double)*3);
  double *satv = (double *) malloc(sizeof(double)*3);
  double *unitlookvector = (double *) malloc(sizeof(double)*3);

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
#pragma omp parallel for shared(demwidth_d, nlines_d, firstlat_d, firstline_d, deltalat_d, firstpix_d, lastpix_d, deg2rad, firstlon_d, deltalon_d, demin_d, a, e2, xyzfit_d, xyz_d) private(line, pixel, lat, xyzoffset, llhlat, llhlon, llhhgt, re)
  for (loop = 0; loop < demwidth_d * nlines_d; loop++){
    line = (int) (loop / demwidth_d);
    pixel = loop - line * demwidth_d;
    lat = firstlat_d + (line + firstline_d) * deltalat_d;
    xyzoffset = ((long long int) line * (long long int) demwidth_d + (long long int) pixel ) * (long long int) 3;
    xyz_d[xyzoffset+0]=0.;
    xyz_d[xyzoffset+1]=0.;
    xyz_d[xyzoffset+2]=0.;

    if (pixel >= firstpix_d && pixel <= lastpix_d){



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
      if(pixel == firstpix_d + (int)((lastpix_d-firstpix_d)/2-1)){
        xyzfit_d[line * 9 + 3]=xyz_d[xyzoffset+0];
        xyzfit_d[line * 9 + 4]=xyz_d[xyzoffset+1];
        xyzfit_d[line * 9 + 5]=xyz_d[xyzoffset+2];
        }
      if(pixel == firstpix_d + 2 * (int)((lastpix_d-firstpix_d)/2-1)){
        xyzfit_d[line * 9 + 6]=xyz_d[xyzoffset+0];
        xyzfit_d[line * 9 + 7]=xyz_d[xyzoffset+1];
        xyzfit_d[line * 9 + 8]=xyz_d[xyzoffset+2];
	}

    }  // end pixel test
  }  // end loop loop and parallel section
}  // end routine setxyz

//  set azoff array
void setazoff(double *coef_d, double *azoff_d, int demwidth_d, int nlines_d, int firstpix_d, int lastpix_d, int aperture_d, int rawdatalines_d){

  int line, pixel;
  double fit, arg;
  long loop;

  for (loop = 0; loop < demwidth_d * nlines_d; loop++){
    line = (int) (loop / demwidth_d);
    pixel = loop - line * demwidth_d;
//    printf("line pixel %d %d\n",line, pixel);

    if (pixel >= firstpix_d && pixel <= lastpix_d){
      arg=((float)(pixel-firstpix_d)/(float)(lastpix_d-firstpix_d)*2.-1.);
      fit=coef_d[line*3+0]*arg*arg+coef_d[line*3+1]*arg+coef_d[line*3+2];
      azoff_d[line * demwidth_d + pixel]=-1;
      if (fit > aperture_d/2 && fit < rawdatalines_d- aperture_d/2){
        azoff_d[line * demwidth_d + pixel]=fit;
      }  // end fit in aperture test
  }  // end pixel test
  }  // end loop loop
}  //end setazoff routine


extern void azimuth_compress_cpu_(
				  float complex *burstdata,
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
  double *xyz, *azoff;
  double tline;
  double rngpix;
  double *xyzfit;
  double *coef;
  double umag, fd, veff, td, udotv;
  int firstline, lastline;  // limits on line loop
  int firstpix, lastpix;  // limits on pixel loop
  long int arraysize;
  FILE *fpout;  // stream for file descriptor *fdout

  int naperture; // naperture is integration midpoint in pixels
  int y1,y2,y3;
  int pixel,line,i,j,ipix;
  int nbytes;
  int nlines;
  off_t iaddr_off_t;
  size_t iaddr_size_t;
  float complex *outdata, *indata;

  // variables for openmp pragma argument passing
  double omptmid=*tmid;
  double ompangc0=*angc0, ompangc1=*angc1;
  double ompprf=*prf;
  int omprawdatalines=*rawdatalines;
  double ompwvl=*wvl;

  // how much memory will we need to allocate?
  //printf("latlons %f %f %f %f\n",latlons[0],latlons[1],latlons[2],latlons[3]);
  lastline=(int)((latlons[0] - *firstlat) / *deltalat );
  firstline=(int)((latlons[1] - *firstlat) / *deltalat );
  if (firstline < 0) firstline=0;
  if (lastline < 0) lastline=0;
  if (firstline > *demlength-1) firstline= *demlength -1;
  if (lastline > *demlength-1) lastline= *demlength -1;
  nlines=lastline-firstline+1;
  printf("Burst line limits, size (first,last,nlines) %d %d %d\n",firstline,lastline,nlines);
  if (nlines == 1){
    printf("Burst not in DEM, skipped.\n");
    return;
  }
  // malloc cpu arrays
  arraysize = (long int) nlines * (long int) *demwidth;
  lon = (double *) malloc(*demwidth * sizeof(double));
  demin = (short *) malloc(arraysize * sizeof(short));
  azoff = (double *) malloc(arraysize * sizeof(double));
  xyz = (double *) malloc(arraysize * sizeof(double) * 3);
  xyzfit = (double *) malloc(nlines * sizeof(double) * 9);
  coef = (double *) malloc(nlines * sizeof(double) * 3);
  outdata = (float complex *)malloc(arraysize * sizeof(float complex));
  indata = (float complex *)malloc(arraysize * sizeof(float complex));
  
  // cpu array definitions
  //  _Complex float *burstdata_d;
  double *satloc_d;
  double *azoff_d, *xyz_d;
  double *xyzfit_d;
  double *coef_d;
  //  _Complex float *outdata_d;
  short *demin_d;

  // constants and such
  double pi;
  pi = 4. * atan2(1.0,1.0);

  // set up longitude loop
  firstpix=(latlons[2]-*firstlon)/ *deltalon; if (firstpix < 0)firstpix=0;
  lastpix=(latlons[3]-*firstlon)/ *deltalon; if (lastpix > *demwidth)lastpix= *demwidth;

  //  define longitude array
  for (i=0;i<*demwidth;i++){
    lon[i]=*firstlon + (i-1)* *deltalon;
  }

  // zero out data array before integration
  for (j=0; j<nlines; j++){
    for (i=0;i<*demwidth;i++){
      outdata[i+j * *demwidth]=0.+0.*I;
      indata[i+j * *demwidth]=0.+0.*I;
    }}

  //  process full burst, begin by grabbing proper section of DEM
  iaddr_off_t=(long long int) firstline * (long long int) *demwidth * (long long int) 2;
  nbytes=lseek(*fddem, iaddr_off_t, SEEK_SET);
  iaddr_size_t= (long long int) nlines * (long long int) *demwidth * (long long int) 2;
  nbytes=read(*fddem,demin,iaddr_size_t);
  //  printf("dem bytes read %ld\n",nbytes);

  if (nbytes < 0) {
    printf("dem read error %d\n",nbytes);
    printf("iaddr_off_t %ld, iaddr_size_t %ld\n",iaddr_off_t, iaddr_size_t);
    printf("nlines %d, demwidth %d\n",nlines,*demwidth);
  }
         
  //  set xyz array in gpu
  setxyz(demin, xyz, xyzfit, azoff, *demwidth, *firstlat, *deltalat, *firstlon, *deltalon, firstline, nlines, firstpix, lastpix);

  // remainder of azoff calcs before integration
  double *xyztemp = (double *) malloc(sizeof(double)*3);
  double *satx = (double *) malloc(sizeof(double)*3);
  double *satv = (double *) malloc(sizeof(double)*3);
  double *unitlookvector = (double *) malloc(sizeof(double)*3);

  //  loop over lines in DEM for burst
  for (line=firstline; line<lastline;line++){

    for (ipix=0; ipix<3; ipix++){
      pixel = firstpix + ipix * (int)((lastpix - firstpix)/2 - 1);

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
         }

    } // end pixel loop
  }  // end line loop

  //  printf("computed parabolas for each line\n");

// compute azoff array in cpu

  setazoff(coef, azoff, *demwidth, nlines, firstpix, lastpix, *aperture, *rawdatalines);

  free(xyztemp);
  free(satx);
  free(satv);
  free(unitlookvector);

  pixelint(burstdata,satloc,xyz,azoff, *demwidth, *aperture, *iaperture, *rawdatalines, *samplesPerBurst, *rngstart, *rngend, *dmrg, *wvl, outdata, nlines);

  // get existing data for that burst
  iaddr_off_t=(long long int) firstline * (long long int) *demwidth * (long long int) 8;
  iaddr_size_t= arraysize * 8;
  //  printf("offset size %lld %lld\n",iaddr_off_t, iaddr_size_t);

  fpout = fdopen(*fdout,"r+");
  nbytes=fseek(fpout, iaddr_off_t, SEEK_SET);
  nbytes=fread(indata, 1, iaddr_size_t, fpout);
  //  printf("bytes read %ld\n",nbytes);

  // update if pixel computed
  for (line=0; line < nlines; line++){
    for (pixel=0; pixel< *demwidth; pixel++){
      if(abs(creal(outdata[line * *demwidth +pixel])) > 1.e-18)indata[line * *demwidth + pixel]=outdata[line * *demwidth + pixel];
    }
  }
  // write line to file
  nbytes=fseek(fpout, iaddr_off_t, SEEK_SET);
  nbytes=fwrite(indata, 1, iaddr_size_t, fpout);

  // free up cpu memory
  free(indata);
  free(outdata);
  free(demin);
  free(xyzfit);
  free(coef);
  free(lon);
  free(xyz);
  free(azoff);
}

