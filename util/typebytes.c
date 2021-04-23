/*  print out/analyze header bytes in a file */

#include <stdio.h>

main(int argc, char *argv[])
{
  unsigned char in[100000];
  char file[60];
  int linelength,headerskip,firstline,nlines,firstpix,npix;
  int i,line,k,ierr;
  FILE *fp;

  /* input arguments */
  if(argc < 7){
    printf("usage:  typebytes infile linelength headerskip firstline(start at 1) nlines firstpix(start at 1) npix\n");
    exit(0);
  }
  strcpy(file,argv[1]);
  sscanf(argv[2],"%d",&linelength);
  sscanf(argv[3],"%d",&headerskip);
  sscanf(argv[4],"%d",&firstline);
  sscanf(argv[5],"%d",&nlines);
  sscanf(argv[6],"%d",&firstpix);
  sscanf(argv[7],"%d",&npix);

  /* open the file */
  if((fp=fopen(file,"r")) == NULL)
  {
  printf("Error opening file,\n");
  exit(0);
  }

  /*  loop over lines  */
  fseek(fp,headerskip+(firstline-1)*linelength,SEEK_SET);
  for (line=firstline; line < firstline+nlines; line++)
    {
      ierr=fread(in,sizeof(unsigned char),linelength,fp);
      if (ierr < linelength)break;
      /*      printf("ierr = %d\n",ierr); */
      printf("%d  ",line);
      for (i=firstpix-1; i<firstpix-1+npix; i++)
	{
	  printf("  %d  ",in[i]&255);
	}
      printf("\n");

      /* i*4 version of data */
      /*  printf("%d  ",line); */
      for (i=firstpix-1; i<firstpix-1+npix; i=i+4)
	{
	  k=(in[i]&255)*256*256*256+(in[i+1]&255)*256*256+(in[i+2]&255)*256+(in[i+3]&255);
	  /*	  printf("  %d  ",k); */
	}
      /*      printf("\n");
	      printf("\n"); */

    }
}


