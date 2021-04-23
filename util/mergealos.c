/*  mergealos - concatenate two preprocessed alos .raw files */

#include <stdio.h>

main(int argc, char *argv[])
{
  unsigned char in[100000];
  char file1[60], file2[60], fileout[60];
  int linelength,headerskip,firstline,nlines,firstpix,npix;
  int i,line,k,ierr,ms,msold,ms2,msold2,writeflag;
  int dw, dwold, dw2, dwold2;
  int count, countold, count2, countold2;
  FILE *fp1, *fp2, *fpout;

  ms=0;
  ms2=0;
  dw=0;
  dw2=0;
  count=0;
  count2=0;

  /* input arguments */
  if(argc < 4){
    printf("usage:  mergealos infile1 infile2 outfile linelength \n");
    exit(0);
  }
  strcpy(file1,argv[1]);
  strcpy(file2,argv[2]);
  strcpy(fileout,argv[3]);
  sscanf(argv[4],"%d",&linelength);
  /*  sscanf(argv[5],"%d",&headerskip); */

  /* open the files */
  if((fp1=fopen(file1,"r")) == NULL)
  {
  printf("Error opening file 1,\n");
  exit(0);
  }
  if((fp2=fopen(file2,"r")) == NULL)
  {
  printf("Error opening file 2,\n");
  exit(0);
  }
  if((fpout=fopen(fileout,"w")) == NULL)
  {
  printf("Error opening output file,\n");
  exit(0);
  }

  /* transfer the header record first */
  /*
  fread(in,sizeof(unsigned char),headerskip,fp1);
  fwrite(in,sizeof(unsigned char),headerskip,fpout);
  */

  /*  loop over lines in input file first */
  for (line=0; line < 1000000; line++)
    {
      ierr=fread(in,sizeof(unsigned char),linelength,fp1);
      if (ierr < linelength)break;
      i=44;
      msold=ms;
      ms=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
      fwrite(in,sizeof(unsigned char),linelength,fpout);
      /* get the data window position in ns */
      i=116;
      dwold=dw;
      dw=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
      i=284;
      countold=count;
      count=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
    }
  printf("Last, penultimate ms of day in file 1= %d %d\n",ms,msold);
  printf("Last, penultimate data window position, ns in file 1= %d %d\n",dw,dwold);
  printf("Last, penultimate frame counter in file 1= %d %d\n",count,countold);

  /* start reading file 2 */
  writeflag=0;
  /*  skip header record */
  /*  fread(in,sizeof(unsigned char),headerskip,fp2); */
  for (line=0; line < 1000000; line++)
    {
      ierr=fread(in,sizeof(unsigned char),linelength,fp2);
      if (ierr < linelength)break;
      i=44;
      msold2=ms2;
      ms2=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
      if((ms==ms2)&&(msold2==msold))
	{
	  writeflag=1;
	  fseek(fpout,-linelength,SEEK_CUR);
	}
      if(writeflag == 1)fwrite(in,sizeof(unsigned char),linelength,fpout);
      /* get the data window position in ns */
      i=116;
      dwold2=dw2;
      dw2=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
      i=284;
      countold2=count2;
      count2=(in[i+3]&255)*256*256*256+(in[i+2]&255)*256*256+(in[i+1]&255)*256+(in[i]&255);
    }
  printf("Last, penultimate ms of day in file 2= %d %d\n",ms2,msold2);
  printf("Last, penultimate data window position, ns in file 2= %d %d\n",dw2,dwold2);
  printf("Last, penultimate frame counter in file 2= %d %d\n",count2,countold2);
}


