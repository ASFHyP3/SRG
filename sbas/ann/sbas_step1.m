% Step one
% Read all coorelation and phase files
% Generate a mask
clear all;clc;
%read amplitude and unwrapped phase values
nr=605;naz=345;% imaga size
n=24;N=n*(n-1)/2;
% correlations
coh=zeros(nr,naz,N);
for kk=1:N
    filename_c=sprintf('/data2/jingyi/igramA/topophase%d.c',kk); 
    fid=fopen(filename_c);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    coh(:,:,kk)=dat((nr+1):end,:);
    fclose(fid);
end
indx=coh(:)>1;
coh(indx)=1;
indx=coh(:)<0;
coh(indx)=0;
coh_mean=mean(coh,3);
indx=coh_mean(:)>0.2;
mask=zeros(nr,naz);
mask(indx)=1;
% InSAR phase
phase=zeros(nr,naz,N);
for kk=1:N
    filename=sprintf('/data2/jingyi/igramA/topophase%d.un',kk);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    phase(:,:,kk)=dat(nr+1:end,:);
    fclose(fid);
end
% amplitude
amp=zeros(nr,naz);
for ii=1:N
    filename=sprintf('/data2/jingyi/igramA/topophase%d.un',ii);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    amp=amp+dat(1:nr,:);
    fclose(fid);
end
amp=amp./N;
save dat mask phase coh amp nr naz n N;