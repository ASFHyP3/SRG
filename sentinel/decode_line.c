#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <math.h>

// some global declarations
int bitptr;
int sampnum;
unsigned int mcode[5][512];
unsigned int bitinc[5][512];
double iqeo[4][65536];
extern FILE *fpbrc;

const double nrl[16][9] = {
  {0., 0.2490, 0.1290, 0.0660, 0.3637, 0.3042, 0.2305, 0.1702, 0.1130},
  {1., 0.7681, 0.3900, 0.1985, 1.0915, 0.9127, 0.6916, 0.5107, 0.3389},
  {2., 1.3655, 0.6601, 0.3320, 1.8208, 1.5216, 1.1528, 0.8511, 0.5649},
  {3., 2.1864, 0.9471, 0.4677, 2.6406, 2.1313, 1.6140, 1.1916, 0.7908},
  {4., -99.99, 1.2623, 0.6061, -99.99, 2.8426, 2.0754, 1.5321, 1.0167},
  {5., -99.99, 1.6261, 0.7487, -99.99, -99.99, 2.5369, 1.8726, 1.2428},
  {6., -99.99, 2.0793, 0.8964, -99.99, -99.99, 3.1191, 2.2131, 1.4687},
  {7., -99.99, 2.7467, 1.0510, -99.99, -99.99, -99.99, 2.5536, 1.6947},
  {8., -99.99, -99.99, 1.2143, -99.99, -99.99, -99.99, 2.8942, 1.9206},
  {9., -99.99, -99.99, 1.3896, -99.99, -99.99, -99.99, 3.3744, 2.1466},
  {10., -99.99, -99.99, 1.5800, -99.99, -99.99, -99.99, -99.99, 2.3725},
  {11., -99.99, -99.99, 1.7914, -99.99, -99.99, -99.99, -99.99, 2.5985},
  {12., -99.99, -99.99, 2.0329, -99.99, -99.99, -99.99, -99.99, 2.8244},
  {13., -99.99, -99.99, 2.3234, -99.99, -99.99, -99.99, -99.99, 3.0504},
  {14., -99.99, -99.99, 2.6971, -99.99, -99.99, -99.99, -99.99, 3.2764},
  {15., -99.99, -99.99, 3.2692, -99.99, -99.99, -99.99, -99.99, 3.6623}
  };

const double sf[256] = {
  0.00, 0.63, 1.25, 1.88, 2.51, 3.13, 3.76, 4.39, 5.01, 5.64, 6.27, 6.89, 
  7.52, 8.15, 8.77, 9.40, 10.03, 10.65, 11.28, 11.91, 12.53, 13.16, 13.79, 
  14.41, 15.04, 15.67, 16.29, 16.92, 17.55, 18.17, 18.80, 19.43, 20.05, 
  20.68, 21.31, 21.93, 22.56, 23.19, 23.81, 24.44, 25.07, 25.69, 26.32, 
  26.95, 27.57, 28.20, 28.83, 29.45, 30.08, 30.71, 31.33, 31.96, 32.59, 
  33.21, 33.84, 34.47, 35.09, 35.72, 36.35, 36.97, 37.60, 38.23, 38.85, 
  39.48, 40.11, 40.73, 41.36, 41.99, 42.61, 43.24, 43.87, 44.49, 45.12, 
  45.75, 46.37, 47.00, 47.63, 48.25, 48.88, 49.51, 50.13, 50.76, 51.39, 
  52.01, 52.64, 53.27, 53.89, 54.52, 55.15, 55.77, 56.40, 57.03, 57.65, 
  58.28, 58.91, 59.53, 60.16, 60.79, 61.41, 62.04, 62.98, 64.24, 65.49, 
  66.74, 68.00, 69.25, 70.50, 71.76, 73.01, 74.26, 75.52, 76.77, 78.02, 
  79.28, 80.53, 81.78, 83.04, 84.29, 85.54, 86.80, 88.05, 89.30, 90.56, 
  91.81, 93.06, 94.32, 95.57, 96.82, 98.08, 99.33, 100.58, 101.84, 103.09, 
  104.34, 105.60, 106.85, 108.10, 109.35, 110.61, 111.86, 113.11, 114.37, 
  115.62, 116.87, 118.13, 119.38, 120.63, 121.89, 123.14, 124.39, 125.65, 
  126.90, 128.15, 129.41, 130.66, 131.91, 133.17, 134.42, 135.67, 136.93, 
  138.18, 139.43, 140.69, 141.94, 143.19, 144.45, 145.70, 146.95, 148.21, 
  149.46, 150.71, 151.97, 153.22, 154.47, 155.73, 156.98, 158.23, 159.49, 
  160.74, 161.99, 163.25, 164.50, 165.75, 167.01, 168.26, 169.51, 170.77, 
  172.02, 173.27, 174.53, 175.78, 177.03, 178.29, 179.54, 180.79, 182.05, 
  183.30, 184.55, 185.81, 187.06, 188.31, 189.57, 190.82, 192.07, 193.33, 
  194.58, 195.83, 197.09, 198.34, 199.59, 200.85, 202.10, 203.35, 204.61, 
  205.86, 207.11, 208.37, 209.62, 210.87, 212.13, 213.38, 214.63, 215.89, 
  217.14, 218.39, 219.65, 220.90, 222.15, 223.41, 224.66, 225.91, 227.17, 
  228.42, 229.67, 230.93, 232.18, 233.43, 234.69, 235.94, 237.19, 238.45, 
  239.70, 240.95, 242.21, 243.46, 244.71, 245.97, 247.22, 248.47, 249.73, 
  250.98, 252.23, 253.49, 254.74, 255.99, 255.99};

void ini_codes(){
  int i, j;

  // initialize tables
  for(i=0;i<512;i++){
    for(j=0;j<5;j++){
      mcode[j][i]=-99;
      bitinc[j][i]=-99;
    }
  }

  // brc 0 table
  for(i=0;i<8;i++){
    if((i&0b100)==0b000)mcode[0][i]=0;
    if((i&0b110)==0b100)mcode[0][i]=1;
    if((i&0b111)==0b110)mcode[0][i]=2;
    if((i&0b111)==0b111)mcode[0][i]=3;
    if((i&0b100)==0b000)bitinc[0][i]=1;
    if((i&0b110)==0b100)bitinc[0][i]=2;
    if((i&0b111)==0b110)bitinc[0][i]=3;
    if((i&0b111)==0b111)bitinc[0][i]=3;
  };

  // brc 1 table
  for(i=0;i<16;i++){
    if((i&0b1000)==0b0000)mcode[1][i]=0;
    if((i&0b1100)==0b1000)mcode[1][i]=1;
    if((i&0b1110)==0b1100)mcode[1][i]=2;
    if((i&0b1111)==0b1110)mcode[1][i]=3;
    if((i&0b1111)==0b1111)mcode[1][i]=4;
    if((i&0b1000)==0b0000)bitinc[1][i]=1;
    if((i&0b1100)==0b1000)bitinc[1][i]=2;
    if((i&0b1110)==0b1100)bitinc[1][i]=3;
    if((i&0b1111)==0b1110)bitinc[1][i]=4;
    if((i&0b1111)==0b1111)bitinc[1][i]=4;
   };

  // brc 2 table
  for(i=0;i<64;i++){
    if((i&0b100000)==0b000000)mcode[2][i]=0;
    if((i&0b110000)==0b100000)mcode[2][i]=1;
    if((i&0b111000)==0b110000)mcode[2][i]=2;
    if((i&0b111100)==0b111000)mcode[2][i]=3;
    if((i&0b111110)==0b111100)mcode[2][i]=4;
    if((i&0b111111)==0b111110)mcode[2][i]=5;
    if((i&0b111111)==0b111111)mcode[2][i]=6;
    bitinc[2][i]=mcode[2][i]+1;
    if(i==63)bitinc[2][i]=6;
  };
  
  // brc 3 table
  for(i=0;i<256;i++){
    if((i&192)==0)mcode[3][i]=0;
    if((i&(128+64))==64)mcode[3][i]=1;
    if((i&(128+64))==128)mcode[3][i]=2;
    if((i&(128+64+32))==128+64)mcode[3][i]=3;
    if((i&(128+64+32+16))==128+64+32)mcode[3][i]=4;
    if((i&(128+64+32+16+8))==128+64+32+16)mcode[3][i]=5;
    if((i&(128+64+32+16+8+4))==128+64+32+16+8)mcode[3][i]=6;
    if((i&(128+64+32+16+8+4+2))==128+64+32+16+8+4)mcode[3][i]=7;
    if((i&(128+64+32+16+8+4+2+1))==128+64+32+16+8+4+2)mcode[3][i]=8;
    if((i&(128+64+32+16+8+4+2+1))==128+64+32+16+8+4+2+1)mcode[3][i]=9;
    if(mcode[3][i]==0)bitinc[3][i]=2;
    if(mcode[3][i]==1)bitinc[3][i]=2;
    if(mcode[3][i]==2)bitinc[3][i]=2;
    if(mcode[3][i]==3)bitinc[3][i]=3;
    if(mcode[3][i]==4)bitinc[3][i]=4;
    if(mcode[3][i]==5)bitinc[3][i]=5;
    if(mcode[3][i]==6)bitinc[3][i]=6;
    if(mcode[3][i]==7)bitinc[3][i]=7;
    if(mcode[3][i]==8)bitinc[3][i]=8;
    if(mcode[3][i]==9)bitinc[3][i]=8;  
  };

  // brc 4 table
  for(i=0;i<512;i++){
    if((i&0b110000000)==0b000000000)mcode[4][i]=0;
    if((i&0b111000000)==0b010000000)mcode[4][i]=1;
    if((i&0b111000000)==0b011000000)mcode[4][i]=2;
    if((i&0b111000000)==0b100000000)mcode[4][i]=3;
    if((i&0b111000000)==0b101000000)mcode[4][i]=4;
    if((i&0b111100000)==0b110000000)mcode[4][i]=5;
    if((i&0b111100000)==0b110100000)mcode[4][i]=6;
    if((i&0b111100000)==0b111000000)mcode[4][i]=7;
    if((i&0b111110000)==0b111100000)mcode[4][i]=8;
    if((i&0b111111000)==0b111110000)mcode[4][i]=9;
    if((i&0b111111110)==0b111111000)mcode[4][i]=10;
    if((i&0b111111110)==0b111111010)mcode[4][i]=11;
    if((i&0b111111111)==0b111111100)mcode[4][i]=12;
    if((i&0b111111111)==0b111111101)mcode[4][i]=13;
    if((i&0b111111111)==0b111111110)mcode[4][i]=14;
    if((i&0b111111111)==0b111111111)mcode[4][i]=15;

    if(mcode[4][i]==0)bitinc[4][i]=2;  
    if(mcode[4][i]==1)bitinc[4][i]=3;  
    if(mcode[4][i]==2)bitinc[4][i]=3;  
    if(mcode[4][i]==3)bitinc[4][i]=3;  
    if(mcode[4][i]==4)bitinc[4][i]=3;  
    if(mcode[4][i]==5)bitinc[4][i]=4;  
    if(mcode[4][i]==6)bitinc[4][i]=4;  
    if(mcode[4][i]==7)bitinc[4][i]=4;  
    if(mcode[4][i]==8)bitinc[4][i]=5;  
    if(mcode[4][i]==9)bitinc[4][i]=6;  
    if(mcode[4][i]==10)bitinc[4][i]=8;  
    if(mcode[4][i]==11)bitinc[4][i]=8;  
    if(mcode[4][i]==12)bitinc[4][i]=9;  
    if(mcode[4][i]==13)bitinc[4][i]=9;  
    if(mcode[4][i]==14)bitinc[4][i]=9;  
    if(mcode[4][i]==15)bitinc[4][i]=9;  
  };
}

void decode_block(int brc, int ncodes, unsigned char *data, int whichquad){

  int b, temp, signbit, startbyte, bitstoshift, bits, mask, signshift, bitshift;

  for(b=0;b<ncodes;b++){
    switch (brc){
    case 0:
      signshift=15;
      bitshift=13;
      mask=7;
      break;
    case 1:
      signshift=15;
      bitshift=12;
      mask=15;
      break;
    case 2:
      signshift=15;
      bitshift=10;
      mask=63;
      break;
    case 3:
      signshift=15;
      bitshift=8;
      mask=255;
      break;
    case 4:
      signshift=31;
      bitshift=23;
      mask=511;
      break;
    }

    startbyte=floor(bitptr/8);
    temp=int2(&data[startbyte]);
    if(brc<4){
      bitstoshift=signshift-(bitptr-startbyte*8);
      signbit=(temp >> bitstoshift)&1;
      bitptr+=1;
      bitstoshift=bitshift-(bitptr-startbyte*8);
      bits= (temp >> bitstoshift)&mask;
      bitptr += bitinc[brc][bits];
    }
    else{
    // for brc=4 need 32 bits
      temp=int4(&data[startbyte]);
      bitstoshift=signshift-(bitptr-startbyte*8);
      signbit=(temp >> signshift)&1;
      bitptr+=1;
      bitstoshift=bitshift-(bitptr-startbyte*8);
      bits= (temp >> bitstoshift)&mask;
      bitptr += bitinc[brc][bits];
    }
    if (signbit == 0)
      iqeo[whichquad][sampnum]=nrl[mcode[brc][bits]][brc+4];
    else
      iqeo[whichquad][sampnum]=-nrl[mcode[brc][bits]][brc+4];
    sampnum++;
  }
}

int get_brc(unsigned char *data){

  int startbyte, temp, bitstoshift;

  startbyte=bitptr/8;
  bitstoshift=13-(bitptr-startbyte*8);
  temp=int2(&data[startbyte]);
  bitptr+=3;
  return (temp >> bitstoshift)&7;
}

int get_thidx(unsigned char *data){

  int startbyte, temp, bitstoshift, i, bits;
  
  startbyte=bitptr/8;
  temp=int2(&data[startbyte]);
  // thidx is in next 8 bits
  bitstoshift=8-(bitptr-startbyte*8);
  bits= (temp >> bitstoshift)&255;
  bitptr+=8;
  return bits;
}

void decode_line(unsigned char *data, int nq, int packetDataLength, float *samples, int linenum){

  int b, brc[500], thidx[500], temp, bits, startbyte, signbit, bitstoshift, block;
  int nblocks, i, sampindex, sampstodecode;
  long q;
  double value;

  ini_codes();

  // how many blocks?
  nblocks=nq/128;

  // start with ie codes
  bitptr=0;
  sampnum=0;
  for(block=0;block<nblocks+1;block++){
    sampstodecode=128;
    if(block==nblocks)sampstodecode=nq-nblocks*128;
    brc[block]=get_brc(data);
    decode_block(brc[block], sampstodecode, data, 0);
  }

  // round up bitptr to 16 word boundary
  if((bitptr%16)!=0)bitptr+=16-(bitptr%16);

  // io codes
  sampnum=0;
  for(block=0;block<nblocks+1;block++){
    sampstodecode=128;
    if(block==nblocks)sampstodecode=nq-nblocks*128;
    startbyte=bitptr/8;
    decode_block(brc[block], sampstodecode, data, 1);
  }

  // round up bitptr to 16 word boundary
  if((bitptr%16)!=0)bitptr+=16-(bitptr%16);

  // qe codes
  sampnum=0;
  for(block=0;block<nblocks+1;block++){
    thidx[block]=get_thidx(data);
    sampstodecode=128;
    if(block==nblocks)sampstodecode=nq-nblocks*128;
    decode_block(brc[block], sampstodecode, data, 2);
  }

  // round up bitptr to 16 word boundary
  if((bitptr%16)!=0)bitptr+=16-(bitptr%16);

  // qo codes
  sampnum=0;
  for(block=0;block<nblocks+1;block++){
    sampstodecode=128;
    if(block==nblocks)sampstodecode=nq-nblocks*128;

    startbyte=bitptr/8;
    decode_block(brc[block], sampstodecode, data, 3);
  }

  // round up bitptr to 16 word boundary
  if((bitptr%16)!=0)bitptr+=16-(bitptr%16);

  // create complex samples
  for(i=1;i<nq+1;i++){
    block=(i-1)/128;
    sampindex=2*i-1;
    samples[sampindex*2-2]=iqeo[0][i-1]*sf[thidx[block]];
    samples[sampindex*2-1]=iqeo[2][i-1]*sf[thidx[block]];
    sampindex=2*i;
    samples[sampindex*2-2]=iqeo[1][i-1]*sf[thidx[block]];
    samples[sampindex*2-1]=iqeo[3][i-1]*sf[thidx[block]];
  }
}

