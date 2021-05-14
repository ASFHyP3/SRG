#include <stdio.h> 
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#define PERMS 0666

int initdk_(lun, file)
int *lun; char *file;
{  int i;
   int fd;
   char filename[100];

   i=0;
   while(file[i]!=' ' && i<strlen(file) && i<99){
     filename[i]=file[i];
     i++;
     filename[i]='\0';
   }
   
   if((fd=open(filename,O_RDWR)) < 0){
       if( (fd = open(filename,O_RDONLY)) > 0)
           printf(" Open filename %s as READ ONLY\n",filename);
   }
   if( fd < 0 ) fd = open(filename,O_CREAT|O_RDWR,0666);
   if(fd == -1)printf(" Cannot open the filename: %s\n",filename);
   return(fd);
}

int iowrit_(chan, buff, bytes)
int *chan, *bytes;
char *buff;
{  
   int nbytes;
   nbytes = write(*chan, buff, *bytes);
   if(nbytes != *bytes) fprintf(stderr,
       " ** Warning: %d bytes written out of %d bytes requested\n",
       nbytes, *bytes);
   return(nbytes);
}

int ioread_(chan, buff, bytes)
int *chan, *bytes ;
char *buff;
{  
   int nbytes;
   nbytes = read(*chan, buff, *bytes);
   if(nbytes != *bytes) fprintf(stderr,
     " ** Warning: only %d bytes read out of %d requested\n",
     nbytes, *bytes);
   return(nbytes);
}

int ioseek_(chan, loc_byte)
int *chan, *loc_byte;

{  
   int nloc;
   off_t ibytes;
   ibytes = (off_t) *loc_byte ;

   if(ibytes >= 0) {nloc = lseek(*chan, ibytes, SEEK_SET);
     /* printf("nloc ibytes *chan %d %d %d\n",nloc,ibytes,*chan); */
   }
   else {
      ibytes = - ibytes;
      nloc = lseek(*chan, ibytes, SEEK_CUR);
   }
   /*   printf("nloc= %d\n",nloc);  */
   return(nloc);
}

int closedk_(lun,chan)
int *lun, *chan;
{
   return(close(*chan));
}



