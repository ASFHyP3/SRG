% Step one
% Read all correlation and phase files
% Generate a mask
clear all; close all; clc;
%read amplitude and unwrapped phase values
nr=512; naz=512;% image size
n=23; % number of slcs 

cells=importdata('flist');
N=length(cells);
%N=50;
phase=zeros(nr,naz,N);
amp=zeros(nr,naz);
coh=zeros(nr,naz,N);

for i=1:N
    strint=cells{i};
    strunw=strrep(strint,'.f','.u');
    stramp=strrep(strint,'.f','.a');
    strcc=strrep(strint,'.f','.c'); 
% correlations
    filename_c=sprintf('%s',strcc); 
    fid=fopen(filename_c);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    coh(:,:,i)=dat((nr+1):end,:);
    fclose(fid);
end
display('Correlations read in.');
for i=1:N
    strint=cells{i};
    strunw=strrep(strint,'.f','.u');
    stramp=strrep(strint,'.f','.a');
    strcc=strrep(strint,'.f','.c');  
% unwrapped igrams    
    filename=sprintf('%s',strunw);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    phase(:,:,i)=dat(nr+1:end,:);
    fclose(fid);
end
display('Unwrapped interferograms read in.');
for i=1:N
    strint=cells{i};
    strunw=strrep(strint,'.f','.u');
    stramp=strrep(strint,'.f','.a');
    strcc=strrep(strint,'.f','.c');  
% amplitudes    
    filename=sprintf('%s',stramp);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    amp=amp+dat(1:2:2*nr-1,:)+dat(2:2:2*nr,:);
    fclose(fid);
end
display('Amplitudes read in.');

% indx=coh(:)>1;
% coh(indx)=1;
% indx=coh(:)<0;
% coh(indx)=0;
% coh_mean=mean(coh,3);
% indx=coh_mean(:)>0.2;
mask=ones(nr,naz);
% mask(indx)=1;

amp=amp./N;

save dat mask phase coh amp nr naz n N;