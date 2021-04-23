/*  shortenaloslines - shorten a preprocessed alos .raw file  */

#include <stdio.h>

main(int argc, char *argv[])
{
  unsigned char in[100000];
  char infile[60], fileout[60];
  int linelength,i,line,ierr,linelengthout;
  FILE *fp, *fpout;

  /* input arguments */
  if(argc < 5){
    printf("usage:  shortenaloslines infile outfile linelengthin linelengthout \n");
    exit(0);
  }
  strcpy(infile,argv[1]);
  strcpy(fileout,argv[2]);
  sscanf(argv[3],"%d",&linelength);
  sscanf(argv[4],"%d",&linelengthout);

  /* open the files */
  if((fp=fopen(infile,"r")) == NULL)
  {
  printf("Error opening input file.\n");
  exit(0);
  }
  if((fpout=fopen(fileout,"w")) == NULL)
  {
  printf("Error opening output file.\n");
  exit(0);
  }

  /* initialize array */
      for(i=0; i<linelengthout*2; i++)
	{
	  in[i]=0;
	}

  /*  loop over lines in input file */
  for (line=0; line < 1000000; line++)
    {
      ierr=fread(in,sizeof(unsigned char),linelength,fp);
      if (ierr < linelength)break;
      /*      for(i=0; i<linelengthout; i++)
	{
	  in[i]=in[i*2];
	  }*/
      fwrite(in,sizeof(unsigned char),linelengthout,fpout);
    }
  printf("Lines processed, output line length= %d %d\n",line,linelengthout);
}


