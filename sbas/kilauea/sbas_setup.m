% sbas setup
% Generate a unity mask
% create a pseudo-amplitude map from one magnitude file
clear all; close all; clc;

nr=720; naz=720;% image size
n=19; % number of slcs 
looks=1;
cohthresh=0.15;

cells=importdata('unwlist');
N=length(cells);
phase=zeros(nr,naz,N);
amp=zeros(nr,naz);
avecc=zeros(nr,naz);

for i=1:N
    strint=cells{i};
%    strunw=strrep(strint,'.f','.u');
    stramp=strrep(strint,'.unw','.amp');
    strcc=strrep(strint,'.unw','.cc'); 
% amplitudes
    filename_a=sprintf('%s',stramp); 
    fid=fopen(filename_a);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    amp=amp+dat(1:2:end,:).^0.3+dat(2:2:end,:).^0.3;
    fclose(fid);
% correlations
    filename_c=sprintf('%s',strcc); 
    fid=fopen(filename_c);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    avecc=avecc+dat((nr+1):end,:)/N;
    fclose(fid);
end
display('Correlations and amplitudes read in.');
for i=1:N
    strint=cells{i};
% unwrapped igrams    
    filename=sprintf('%s',strint);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    phase(:,:,i)=dat(nr+1:end,:);
    fclose(fid);
end
display('Unwrapped interferograms read in.');

mask=ones(nr,naz)/2;
mask(find(avecc>cohthresh))=1;

disp('Saving measurements into sbas_data');

save sbas_data phase mask amp nr naz n N avecc;

% From time and baseline
load Tm.out
load Bperp.out
load deltime.out
% wavelength
lambda=0.235;%meters
save sbas_info Tm deltime Bperp lambda;


