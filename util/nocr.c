#include <stdio.h>

main(int argc, char* argv[]){
  FILE *fid, *fid2;
  int i;
  char ch;

const char* command = argv[0];
  if (argc != 3)
    {
      printf("Usage: nocr in_file_name out_file_name\n");
      exit(-1);
    }

  int clidx = 1;
  char* lbdr_filename_in=argv[clidx++];
  char* lbdr_filename_out=argv[clidx++];

  fid=fopen(lbdr_filename_in,"r");
  fid2=fopen(lbdr_filename_out,"w");
  for (i=0;i<100000;i++){
    ch=getc(fid);
    /*    printf("%s  %d\n",ch,ch);  */
    if((unsigned int)ch != 13 && ch != -1)putc(ch,fid2);
  }
}
