clear all;clc;
% from step1, taking multiple looks
load dat;
% Average 
looks=1;

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

fid=fopen('sbas_list','r');
str=fscanf(fid,'%s %s %f %f');
str

% From tsxtime and tsxbaseline
load Tm.out
load Bperp.out
load deltime.out
% wavelength
lambda=0.03;%meters
save tsx_info Tm deltime Bperp lambda;


