#include <stdlib.h>
#include <string.h>
#include "tiffio.h"

main(int argc,char **argv)
{
  FILE *outfp, *rscfp, *floatoutfp, *qfp;
  char st[30],infile[300];
  if ( argc <3 ){
    printf("Usage: coptiffread dem.tif dem_i2_file\n");
    exit(0);
  }

  TIFF* tif = TIFFOpen(argv[1],"r");
  if (tif) {
    uint32 w, h, i, j, tw, th, q;
    float* buffer;
    uint32 line, linebytes, samp, tileindex, outindex, bytes;
    int16* data;
    int pos;
    double lat, lon, xstep, ystep;
    char *ptr[300],strlat[5],strlon[5];

    outfp=fopen(argv[2],"w");
    sprintf(st,"%s.rsc",argv[2]);
    rscfp=fopen(st,"w");
  
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);  // get file size info
    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
    TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tw);  // get tile size info
    TIFFGetField(tif, TIFFTAG_TILELENGTH, &th);

    // compute xstep and ystep
    xstep=1./w;
    ystep=-1./h;
    //  strip lat/lon from filename
    memcpy(infile,argv[1],strlen(argv[1]));
    pos=strstr(infile,"COG_10_")-infile+6;
    memcpy(&strlat,&infile[pos+1],3);
    strlat[4]=0;
    printf("strlat %.3s %d %d %d\n",strlat,strlat[0],strlat[1],strlat[2]);
    lat=(strlat[1]-48)*10+(strlat[2]-48);
    if (strlat[0] == 'S'){
      lat=-lat;
      printf("Latitude south\n");
    }
    lat = lat + 1.0 - ystep/2.;
    printf("Latitude %f\n",lat);

    pos=strstr(infile,"_00_")-infile;
    memcpy(&strlon,&infile[pos+4],4);
    strlon[5]=0;
    printf("strlon %.4s %d %d %d %d\n",strlon,strlon[0],strlon[1],strlon[2],strlon[3]);
    if (strlon[0] == 'E')
      lon=(strlon[1]-48)*100+(strlon[2]-48)*10+(strlon[3]-48);
    else
      lon=-((strlon[1]-48)*100+(strlon[2]-48)*10+(strlon[3]-48));

    lon = lon - xstep/2.;
    printf("Longitude %f\n",lon);

    buffer=_TIFFmalloc(TIFFTileSize(tif));  // allocate tiff and slc buffers
    data=malloc(w*h*sizeof(int16)); 
    
    for (line=0;line<h;line += th)
      for (samp=0;samp<w;samp+=tw)
      {
	//	printf("reading tile line samp %d %d\n",line,samp);
        bytes=TIFFReadTile(tif, buffer, samp, line,(uint32) 0,(tsample_t) 0); // read tiff line

	// convert tile to int16
	for (j=0;j<th;j++) //  loop over lines in tile
	  for (i=0;i<tw;i++)  //  loop over samples in tile
	  {
	    tileindex=i+j*tw;
	    if(i+samp<w && j+line < h){
	    outindex=i+samp+(j+line)*w;
	    data[outindex]=buffer[tileindex];  
	    }
	  }
      }
    fwrite(data,sizeof(int16),w*h,outfp); 
    printf("write lines, pixels: %d %d\n",w,h);
    
    TIFFClose(tif);
    fclose(outfp);

    //  now the rsc file
    fprintf(rscfp,"WIDTH          ");fprintf(rscfp,"%d\n",w);
    fprintf(rscfp,"FILE_LENGTH    ");fprintf(rscfp,"%d\n",h);
    fprintf(rscfp,"X_FIRST        ");fprintf(rscfp,"%-15.10f\n",lon);    
    fprintf(rscfp,"Y_FIRST        ");fprintf(rscfp,"%-15.10f\n",lat);    
    fprintf(rscfp,"X_STEP         ");fprintf(rscfp,"%-15.12f\n",xstep);     
    fprintf(rscfp,"Y_STEP         ");fprintf(rscfp,"%-15.12f\n",ystep);     
    fprintf(rscfp,"X_UNIT         degrees\n")      ;
    fprintf(rscfp,"Y_UNIT         degrees\n")      ;
    fprintf(rscfp,"Z_OFFSET       0      \n")      ;
    fprintf(rscfp,"Z_SCALE        1      \n")      ;
    fprintf(rscfp,"PROJECTION     LL     \n")      ;
    fclose(rscfp);

  }
}


