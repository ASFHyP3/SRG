% sbas setup
% Generate a unity mask
% create a pseudo-amplitude map from one magnitude file
clear all; close all; clc;

nr=873; naz=502;% image size
n=8; % number of slcs 
looks=1;
cohthresh=0.15;

cells=importdata('ulist');
N=length(cells);
phase=zeros(nr,naz,N);
amp=zeros(nr,naz);
avecc=zeros(nr,naz);

for i=1:N
    strint=cells{i};
%    strunw=strrep(strint,'.f','.u');
    stramp=strrep(strint,'.u','.a');
    strcc=strrep(strint,'.u','.c'); 
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

% % Average if more looks needed
% 
% if looks==1
%     nr_s=floor(nr);
%     naz_s=floor(naz);
%     mask_s=mask;
%     amp_s=amp;
%     phase_s=phase;
% else
%     nr_s=floor(nr/looks);
%     naz_s=floor(naz/looks);
%     mask_s=zeros(nr_s,naz_s);
%     amp_s=zeros(nr_s,naz_s);
%     phase_s=zeros(nr_s,naz_s,N);
%     for ii=1:nr_s
%         for jj=1:naz_s
%             temp=mask((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks);
%             mask_s(ii,jj)=round(mean(temp(:)));
%             temp=amp((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks);
%             amp_s(ii,jj)=round(mean(temp(:)));
%             temp=phase((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks,:);
%             phase_s(ii,jj,:)=squeeze(sum(sum(temp,1),2))./looks/looks;
%         end
%     end
% end

disp('Saving measurements into sbas_data');

save sbas_data phase mask amp nr naz n N avecc;

% From time and baseline
load Tm.out
load Bperp.out
load deltime.out
% wavelength
lambda=0.03;%meters
save sbas_info Tm deltime Bperp lambda;


