% Step one
% Read phase files only
% Generate a unity mask
% create a pseudo-amplitude map from one magnitude file
clear all; close all; clc;

nr=400; naz=860;% image size
n=17; % number of slcs 
looks=1;

cells=importdata('unwfilelist');
N=length(cells);
phase=zeros(nr,naz,N);
amp=ones(nr,naz);
coh=ones(nr,naz,N);

% for i=1:N
%     strint=cells{i};
% %    strunw=strrep(strint,'.f','.u');
% %    stramp=strrep(strint,'.f','.a');
% %    strcc=strrep(strint,'.f','.c'); 
% % correlations
% %    filename_c=sprintf('%s',strcc); 
% %    fid=fopen(filename_c);
% %    dat=fread(fid,[2*nr,inf],'float','ieee-le');
% %    coh(:,:,i)=dat((nr+1):end,:);
% %    fclose(fid);
% end
display('Correlations not read in.');
for i=1:N
    strint=cells{i};
%    strunw=strrep(strint,'.f','.u');
%    stramp=strrep(strint,'.f','.a');
%    strcc=strrep(strint,'.f','.c');  
% unwrapped igrams    
    filename=sprintf('%s',strint);  
    fid=fopen(filename);
    dat=fread(fid,[2*nr,inf],'float','ieee-le');
    phase(:,:,i)=dat(nr+1:end,:);
    fclose(fid);
end
display('Unwrapped interferograms read in.');
% for i=1:N
%     strint=cells{i};
% %    strunw=strrep(strint,'.f','.u');
% %    stramp=strrep(strint,'.f','.a');
% %    strcc=strrep(strint,'.f','.c');  
% % amplitudes    
% %    filename=sprintf('%s',stramp);  
% %    fid=fopen(filename);
% %    dat=fread(fid,[2*nr,inf],'float','ieee-le');
% %    amp=amp+dat(1:2:2*nr-1,:)+dat(2:2:2*nr,:);
% %    fclose(fid);
% end
amp=dat(1:nr,:).^0.3;
display('Amplitudes from last unwrapped interferogram.');

% indx=coh(:)>1;
% coh(indx)=1;
% indx=coh(:)<0;
% coh(indx)=0;
% coh_mean=mean(coh,3);
% indx=coh_mean(:)>0.2;
mask=ones(nr,naz);
% mask(indx)=1;

%save dat mask phase coh amp nr naz n N;
%clear all;clc;
% from step1, taking multiple looks
%load dat;
% Average 

if looks==1
    nr_s=floor(nr);
    naz_s=floor(naz);
    mask_s=mask;
    amp_s=amp;
    phase_s=phase;
else
    nr_s=floor(nr/looks);
    naz_s=floor(naz/looks);
    mask_s=zeros(nr_s,naz_s);
    amp_s=zeros(nr_s,naz_s);
    phase_s=zeros(nr_s,naz_s,N);
    for ii=1:nr_s
        for jj=1:naz_s
            temp=mask((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks);
            mask_s(ii,jj)=round(mean(temp(:)));
            temp=amp((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks);
            amp_s(ii,jj)=round(mean(temp(:)));
            temp=phase((ii-1)*looks+1:ii*looks,(jj-1)*looks+1:jj*looks,:);
            phase_s(ii,jj,:)=squeeze(sum(sum(temp,1),2))./looks/looks;
        end
    end
end

%  add coherence mask data
avecc=mean(coh,3);

disp('Saving measurements into dat_looks');

save dat_looks phase_s mask_s amp_s nr_s naz_s looks n N avecc;
% save tsxbaseline and time information

% From tsxtime and tsxbaseline
load Tm.out
load Bperp.out
load deltime.out
% wavelength
lambda=0.24;%meters
save tsx_info Tm deltime Bperp lambda;


