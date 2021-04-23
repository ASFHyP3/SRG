#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <math.h>

int int2(unsigned char *data){
  return data[1]+256*data[0];
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
unsigned int3(unsigned char *data){
  return data[2]+data[1]*256+data[0]*256*256;
}
long long int8(unsigned char *data){
  long long temp[1];
  unsigned char tmp, d[8];
  memcpy(d,data,8);
  tmp=d[0];d[0]=d[7];d[7]=tmp;
  tmp=d[1];d[1]=d[6];d[6]=tmp;
  tmp=d[2];d[2]=d[5];d[5]=tmp;
  tmp=d[3];d[3]=d[4];d[4]=tmp; 
  memcpy(temp,d,8);
  return temp[0];
}
double double8(unsigned char *data){
  double temp[1];
  unsigned char tmp, d[8];
  memcpy(d,data,8);
  tmp=d[0];d[0]=d[7];d[7]=tmp;
  tmp=d[1];d[1]=d[6];d[6]=tmp;
  tmp=d[2];d[2]=d[5];d[5]=tmp;
  tmp=d[3];d[3]=d[4];d[4]=tmp; 
  memcpy(temp,d,8);
  return temp[0];
}
