% test sentinel rc azimuth spectrum
clear all;close all;

%  read burst
fp=fopen('burst8','r');
data=fread(fp,[17301*2 inf],'float32');
cpx=data(1:2:end,:)+j*data(2:2:end,:);
cpx(1:9,:)=complex(0.,0.);
subplot(4,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);
title('burst');

% focussed processor
prf=1685.0;
npts=100;
range=900000.;
v=6780;
rate=2*v*v/0.0566/range;
time=linspace(-npts/2/prf,npts/2/prf,npts+1);
phase=zeros(1,size(cpx,2));
phase(1:npts+1)=pi()*rate*time.*time;
ref=complex(cos(phase),sin(phase));
refspec=repmat(fft(ref),size(cpx,1),1);

spec=fft(cpx,[],2);
% plot data spectrum
subplot(4,1,2);
imagesc(abs(spec)'); colorbar; colormap gray;
ave=mean(mean(abs(spec)));
caxis([0 ave*3]);
title('data spectrum');

foc=refspec.*spec;

subplot(4,1,3);
imagesc(abs(foc)'); colorbar; colormap gray;
ave=mean(mean(abs(foc)));
caxis([0 ave*3]);
title('Focussed');

%  looks for display
n=20;
mask=ones(n);
q=conv2(abs(foc),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,4);
imagesc(looks');
colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);
title('Focussed with looks');
